{-|
Lift local functions to the top level.
  1. Free local variables become parameters
  2. Any type variables that are mentioned become type parameters
  3. Any constraints that mention these variables.
  3.5 Note that if these constraints mention variables that are not mentioned
      anywhere else we need to also add *them*, which in turn might add more
      constraints, etc.
  4. Each use of the local function needs to pass these additional
     parameters.
  5. Note that locally defined functions (i.e., non-parameter ones)
     do not become paramters because they will be lifter.

In summary, given: `e where ds` and the `ds` are in dependency order.
  For each `d` in order:
    * rewrite definition to eliminte any functions that were already lifted.
    * compute free variables (types, values, predicates)
    * using this add a new top-level declaration.
    * remember how to change the instantiations, in the continuation
  Once we've done all the `ds` we need to rewrite the `e`

f @ts @ps xs  ~>   f @(freeTs++ts) @(newPs ++ ps) (freeVars ++ xs)

In a smillar way we also need to lift lambdas, except that they are only
goind to be used in place.

When we are a lifting a polymorphic local, we should make sure to not
add constraints that the function already has, so in the above example,
the (++) in the constratins is a bit more complicated.
Similarly, in the definition of the lifted function we have to insert the
correct number of PAbs.

[Local Polymorphic Values]
Note that we also lambda lift local polymorphic values, which hopefully
should not be very common as we have local mono binds on by default.
-}
module Cryptol.IR.LambdaLift (llModule) where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(isJust,catMaybes)
import qualified Data.Text as Text

import MonadLib

import Cryptol.Utils.Panic(panic)
import Cryptol.Utils.PP
import Cryptol.Utils.RecordMap
import Cryptol.Utils.Ident(mkIdent,identText)
import Cryptol.Parser.Position(emptyRange)
import Cryptol.TypeCheck.AST
import Cryptol.TypeCheck.TypeOf(fastTypeOf)
import Cryptol.ModuleSystem.Name(mkDeclared
                                , Supply, FreshM(..), runSupply
                                , nameInfo,NameInfo(..)
                                , NameSource(SystemName)
                                , ModPath, Namespace(NSValue)
                                , nameModPath, nameIdent
                                )
import Cryptol.IR.FreeVars(freeVars,Deps(..))

{-
import Debug.Trace
-}

llModule ::
  Map Name Schema ->
  Supply ->
  ModuleG mname ->
  ((ModuleG mname, Map Name Schema), Supply)
llModule topTypes su mo = ((mo { mDecls = concat dgs }, mconcat tys), newSu)
  where
  (results, newSu) = runSupply su (mapM (llTopDeclGroup topTypes) (mDecls mo))
  (dgs,tys) = unzip results


llTopDeclGroup ::
  FreshM m => Map Name Schema -> DeclGroup -> m ([DeclGroup], Map Name Schema)
llTopDeclGroup topTypes dg =
  case dg of
    NonRecursive d ->
      do (d', newDs) <- llTopDecl topTypes d
         let newTys = Map.fromList [ (dName de, dSignature de)
                                   | g <- newDs, de <- groupDecls g ]
         pure (reverse (NonRecursive d' : newDs), newTys)
    Recursive ds ->
      do (ds',dds) <- mapAndUnzipM (llTopDecl topTypes) ds
         let newDs = concatMap groupDecls (concat dds)
         let newTys = Map.fromList [ (dName de, dSignature de) | de <- newDs ]
         pure ([ Recursive (ds' ++ newDs) ], newTys)

-- | Returns declarations in reversed order
llTopDecl :: FreshM m => Map Name Schema -> Decl -> m (Decl, [DeclGroup])
llTopDecl topTypes decl =
  case dDefinition decl of
    DExpr e ->
      runLiftM (nameModPath (dName decl)) topTypes $
      pushName (Just (dName decl))
      do let (tps, pps, ps, body) = exprToFun e
         body' <- withTParams (Set.fromList tps)
                $ withLocals (Map.fromList [ (x,tMono t) | (x,t) <- ps ])
                $ ll body
         rw <- get
         unless (Set.null (extraTParams rw))
                (panic "llTopDecl" ["Unexpected extra type parameters"])
         unless (Map.null (extraParams rw))
                (panic "llTopDecl" ["Unexpected extra parameters"
                                   , show ("Function: " <+> pp (dName decl))
                                   , show ("Extra: " <+> commaSep (map pp (Map.keys (extraParams rw))))
                                   ])

         let newD = decl { dDefinition = DExpr (mkLam tps pps ps body') }
         pure (newD, newDecls rw)

    DForeign {} -> pure (decl, [])
    DPrim {} -> pure (decl, [])


--------------------------------------------------------------------------------

runLiftM :: FreshM m => ModPath -> Map Name Schema -> M a -> m a
runLiftM mp topTypes m =
  liftSupply \sup ->
    let (a,rw) = runId
               $ runStateT (initRW sup)
               $ runReaderT (initRO mp topTypes) m
    in (a, nameSupply rw)

initRO :: ModPath -> Map Name Schema -> RO
initRO mp topTypes =
  RO
    { modPath          = mp
    , topVars          = topTypes
    , outerVars        = mempty
    , outerConstraints = mempty
    , outerFuns        = mempty
    , localTParams     = mempty
    , localConstraints = mempty
    , localVars        = mempty
    }

type M = ReaderT RO (StateT RW Id)

data RO = RO
  { modPath         :: ModPath
    -- ^ Current ModPath, used when we make new names for the lifted functions.

  , topVars         :: Map Name Schema
    -- ^ Types of top-level declaraitions, so we can compute types of things.

  , outerVars       :: Map Name Schema
    -- ^ Locals in outer functions.

  , outerConstraints :: [Prop]
    -- ^ Constraints in outer functions.

  , outerFuns :: [Maybe Name]
    -- ^ Names of enclosing functions.  We use this to pick a name for
    -- the lambda-lifted things.

  , localTParams  :: Set TParam
    -- ^ Type parameters for the current function.
    -- We don't need to capture these type variables.

  , localConstraints :: [Prop]
    -- ^ Constraints on local function


  , localVars     :: Map Name Schema
    -- ^ Locals defined in the current functions.
    -- These ones we don't need to capture.

  }



initRW :: Supply -> RW
initRW sup =
  RW
    { nameSupply   = sup
    , liftedVars   = mempty
    , extraTParams = mempty
    , extraParams  = mempty
    , newDecls     = mempty
    }

data RW = RW
  { nameSupply    :: Supply
    -- ^ For genearting names for the lifted functions.

  , liftedVars    :: Map Name (Name,[Type],Int,[(Name,Type)])
    -- ^ Lifted functions.  We don't need to capture these.
    -- For each, besided the name of the lifted function we store the
    -- additional variables corresponding to the free variables
    -- (types, props, args) it needs.

  , extraTParams  :: Set TParam
    -- ^ Captured type parameters.

  , extraParams   :: Map Name Type
    -- ^ Captured variables that we depend on.
    -- For each, we keep track of the type of the instantiation.

  , newDecls :: [DeclGroup]
    -- ^ Generated top-level declarations.

  }

newName :: Maybe Name -> M Name
newName mbNm =
  do mp   <- modPath <$> ask
     os <- outerFuns <$> ask
     sets \rw ->
       case mkDeclared NSValue mp SystemName (i os) Nothing r (nameSupply rw) of
         (x, newSup) -> (x, rw { nameSupply = newSup })
  where
  maybeName m =
    case m of
      Nothing -> "lam"
      Just x  -> identText (nameIdent x)

  i outer =mkIdent (Text.intercalate "_"
                        ("ll" : map maybeName (outer ++ [mbNm])))
  r = emptyRange


-- | Since we don't suport recrusive functions at the momennt,
-- we just emit a non-recursive component. This would have to change
-- a bit when we fix up the recursive function support.
addDecl :: Decl -> M ()
addDecl d = sets_ \rw -> rw { newDecls = NonRecursive d : newDecls rw }


withLocals :: Map Name Schema -> M a -> M a
withLocals xs =
  mapReader \ro -> ro { localVars = Map.union xs (localVars ro) }

withTParams :: Set TParam -> M a -> M a
withTParams xs =
  mapReader \ro -> ro { localTParams = Set.union xs (localTParams ro) }

getNewProps ::
  [Prop]      {- ^ Constraints from outer scope -} ->
  Set TParam  {- ^ Type variables to the functions -} ->
  [Prop]      {- ^ Known constraints on function -} ->
  (Set TParam, [Prop]) -- ^ Additional type parameters and constratints
getNewProps outProps knownTVars knownProps =
  go False [] knownTVars knownProps
                            [ (p, tyParams (freeVars p)) | p <- outProps ]
  where
  isKnownProp :: [Prop] -> Prop -> Bool
  isKnownProp new p = p `elem` new || p `elem` knownProps

  haveCommon xs ys = not (Set.null (Set.intersection xs ys))

  keepProp newVars newProps (p,vs) =
    not (isKnownProp newProps p) &&
      (haveCommon vs knownTVars || haveCommon vs newVars)


  go changes doneProps !vs ps todo =
    case todo of
      [] | changes    -> go False [] vs ps doneProps
         | otherwise  -> (vs,ps)

      prop@(p,pvs) : more
        | keepProp vs ps prop ->
          let new    = Set.difference pvs vs
              hasNew = not (Set.null new)
          in if hasNew
               then go True doneProps (new <> vs) (p : ps) more
               else go changes doneProps vs (p : ps) more
        | otherwise -> go changes (prop : doneProps) vs ps todo



pushName :: Maybe Name -> M a -> M a
pushName mb = mapReader \ro -> ro { outerFuns = mb : outerFuns ro }


enterFun ::
  [TParam] ->
  [Prop] ->
  [(Name,Type)] ->
  M a ->
  M ([TParam], [Prop], [(Name, Type)], a)
enterFun tps props params m =
  do (outTPs, outPs) <- sets \rw -> ( (extraTParams rw, extraParams rw)
                                    , rw { extraTParams = mempty
                                         , extraParams  = mempty
                                         }
                                    )
     a <- mapReader (\ro -> ro { outerVars = Map.union (localVars ro)
                                                       (outerVars ro)
                               , outerConstraints = localConstraints ro ++
                                                    outerConstraints ro
                               , localTParams = Set.fromList tps
                               , localConstraints = props
                               , localVars = Map.fromList
                                            [ (x,tMono t) | (x,t) <- params ]
                               }) m

     ro <- ask
     let outCts = outerConstraints ro
     sets \rw ->
       let (newTPs,newProps) = getNewProps outCts (extraTParams rw) props
           newPs             = extraParams rw
       in ( ( Set.toList newTPs
            , newProps
            , Map.toList  newPs
            , a
            )

          , let newToUsTPs = newTPs `Set.difference` localTParams ro
                newToUsParams = newPs `Map.difference` localVars ro
            in
            rw { extraTParams = newToUsTPs <> outTPs
               , extraParams  = newToUsParams <> outPs
               }
          )


-- | Record a dependency on some type parameters.
useTParams :: Set TParam -> M ()
useTParams xs =
  do known <- localTParams <$> ask
     sets_ \rw -> rw { extraTParams = (xs `Set.difference` known) `Set.union`
                                      extraTParams rw }



-- | Record a dependency on a variable.
useVar :: Name -> Type -> M ()
useVar x t0 =
  do t       <- ll t0
     outer   <- outerVars <$> ask
     liftMap <- liftedVars <$> get
     case nameInfo x of
       LocalName {}
          | not (x `Map.member` liftMap) && x `Map.member` outer ->
            sets_ \rw -> rw { extraParams = Map.insert x t (extraParams rw) }
       _ -> pure ()


getExprType :: Expr -> M Type
getExprType e =
  do ro <- ask
     let env = localVars ro <> outerVars ro <> topVars ro
{-
     traceM $ show
            $ debugShowUniques
            $ vcat [ "=== getExprType ================"
                   , "OUTER"
                   , ppMap (outerVars ro)
                   , "LOCAL"
                   , ppMap (localVars ro)
                   , "EXPR"
                   , pp e
                   ]
     traceM $ show $ vcat
                   [ "TYPE"
                   , pp (fastTypeOf env e)
                   , "================================"
                   ]
-}
     pure $! fastTypeOf env e
{-
  where
  ppMap mp = vcat [ pp x <.> ":" <+> pp t | (x,t) <- Map.toList mp ]
-}

--------------------------------------------------------------------------------

-- | Split up the parameters away from an expression.
-- For non-function cases all parameters will be empty
exprToFun :: Expr -> ([TParam], [Prop], [(Name,Type)], Expr)
exprToFun expr =
  let (tps, e1) = splitWhile splitTAbs expr
      (pps, e2) = splitWhile splitProofAbs e1
      (aps, e3) = splitWhile splitAbs e2
  in (tps, pps, aps, e3)


-- | Split up an expression into a call.
-- Note the at this looses some location annotations.
exprToCall :: Expr -> (Expr, [Type], Int, [Expr])
exprToCall expr = (e3, reverse ts, length ps, reverse args)
  where
  (args,  e1) = splitWhile splitApp expr
  (ps,    e2) = splitWhile splitProofApp e1
  (ts,    e3) = splitWhile splitTApp e2

class LL a where
  ll :: a -> M a

instance LL Type where
  ll t = useTParams (tyParams (freeVars t)) >> pure t
  -- XXX: currently we are reusing the names of the outer capture type,
  -- which I think is OK.   If this leads to a problem, we can change this
  -- function to traverse the type and generate new names for used parameters,
  -- we just need to change the monad to use a Map associating known free
  -- variables with their fresh version.

instance LL a => LL [a] where
  ll = traverse ll

instance (LL a, LL b) => LL (a,b) where
  ll (a,b) = (,) <$> ll a <*> ll b

instance LL a => LL (RecordMap k a) where
  ll = traverse ll

instance LL Selector where
  ll = pure

instance LL CaseAlt where
  ll (CaseAlt xs e) =
    do xs' <- traverse (\(x,t) -> (,) x <$> ll t) xs
       withLocals (Map.fromList [ (x,tMono y) | (x,y) <- xs' ])
                  (CaseAlt xs' <$> ll e)

instance LL Expr where
  ll expr =
    case expr of
      ELocated r e -> ELocated r <$> ll e
      EList es t   -> EList <$> ll es <*> ll t
      ETuple es    -> ETuple <$> ll es
      ERec rmap    -> ERec <$> ll rmap
      ESel e s     -> ESel <$> ll e <*> ll s
      ESet rect rec sel val -> ESet <$> ll rect <*> ll rec <*> ll sel <*> ll val
      EIf e1 e2 e3 -> EIf <$> ll e1 <*> ll e2 <*> ll e3
      EPropGuards alts t -> EPropGuards <$> ll alts <*> ll t
      ECase e alts dflt -> ECase <$> ll e <*> traverse ll alts
                                          <*> traverse ll dflt

      -- Order is important!
      EComp tLen tEl e mss ->
        do newMSS <- traverse llSeq mss
           EComp <$> ll tLen <*> ll tEl <*> withLocals (defs mss) (ll e)
                                        <*> pure newMSS

      ETApp {}      -> doCall
      EApp {}       -> doCall
      EProofApp {}  -> doCall
      EVar {}       -> doCall

      ETAbs {}      -> doFun
      EAbs {}       -> doFun
      EProofAbs {}  -> doFun

      -- Order is important!
      EWhere e ds   ->
        do newDs <- llSeq ds
           mbWhere newDs <$> withLocals (defs ds) (ll e)
        where
        mbWhere newDs x =
          case newDs of
            [] -> x
            _  -> EWhere x newDs

    where
    doFun = callLambda expr [] 0 []

    doCall =
      do let (fun, tyArgs0, proofArgs, args) = exprToCall expr
         tyArgs  <- mapM ll tyArgs0
         newArgs <- mapM ll args

         ty <- getExprType (mkCall fun tyArgs0 proofArgs [])
         let needCall = not (null tyArgs)
                     || proofArgs > 0
                     || isJust (tIsFun ty)

         case dropLocs fun of

           -- Variable
           EVar x
             | needCall ->
               do liftMap <- liftedVars <$> get
                  case Map.lookup x liftMap of
                    Just (topFun,moreTs0,morePs,moreXs) ->
                      do moreTs <- mapM ll moreTs0
                         mapM_ (uncurry useVar) moreXs
                         pure (mkCall (EVar topFun)
                                      (moreTs ++ tyArgs)
                                      (morePs + proofArgs)
                                      (map (EVar . fst) moreXs ++ newArgs))


                    Nothing ->
                      do useVar x ty
                         pure (mkCall fun tyArgs proofArgs newArgs)
             | otherwise ->
               do useVar x ty
                  pure (mkCall fun tyArgs proofArgs newArgs)

           -- Lambda
           _ -> callLambda fun tyArgs proofArgs newArgs


callLambda :: Expr -> [Type] -> Int -> [Expr] -> M Expr
callLambda fun tyArgs proofArgs args =
  do (topFun,moreTs,morePs,moreXs) <- doLiftFun Nothing fun
     pure (mkCall (EVar topFun)
                  (moreTs ++ tyArgs)
                  (morePs + proofArgs)
                  (map (EVar . fst) moreXs ++ args))


-- | Sytntactiacally make a function call
mkCall :: Expr -> [Type] -> Int -> [Expr] -> Expr
mkCall f ts p xs =
  case ts of
    [] ->
      case p of
        0 ->
          case xs of
            [] -> f
            x : more -> mkCall (EApp f x) [] 0 more
        _ -> mkCall (EProofApp f) [] (p-1) xs
    t : more -> mkCall (ETApp f t) more p xs

mkLam :: [TParam] -> [Prop] -> [(Name,Type)] -> Expr -> Expr
mkLam tps pps ps body = foldr ETAbs withProofs tps
  where
  withProofs = foldr EProofAbs withArgs pps
  withArgs   = foldr (uncurry EAbs) body ps



-- | Lift a bunch of things, where each thing can bind things in the next,
-- and may also disappear.
llSeq :: (LLMaybe a, Defs a) => [a] -> M [a]
llSeq xs =
  case xs of
    [] -> pure []

    -- Order here is important, because the continuation needs to know
    -- if a something got lifted away.
    m : more -> mbCons <$> llMaybe m <*> withLocals (defs m) (llSeq more)

mbCons :: Maybe a -> [a] -> [a]
mbCons mb xs =
  case mb of
    Nothing -> xs
    Just x  -> x : xs

-- | Lambda lifting for things that might disappear, because they got
-- lifted.  In that case we reutrn `Nothing` and update the state.
class LLMaybe a where
  llMaybe :: a -> M (Maybe a)

instance LLMaybe Match where
  llMaybe mat =
    case mat of
      From x t1 t2 e -> Just <$> (From x <$> ll t1 <*> ll t2 <*> ll e)
      Let d          -> fmap Let <$> llMaybe d

instance LLMaybe DeclGroup where
  llMaybe dg =
    case dg of
      NonRecursive d -> fmap NonRecursive <$> llMaybe d
      Recursive ds ->
        withLocals (defs ds)
        if any isFunDecl ds
          then panic "llMaybe@DeclGroup"
                 ["Lifting mutually recursive functions is not yet implemented"]
          -- XXX: these happen at the same time,
          -- so we need special handling so that the members of the
          -- recursive group call each other correctly.
          -- Furthermore, free variables in any member, should become
          -- be parameters to all members.
          -- We can do this with `mfix` by passing in lazyly the collected extra
          -- vars.
          else Just . Recursive . catMaybes <$> mapM llMaybe ds
                                           {- These should all be Just! -}
doLiftFun :: Maybe Name -> Expr -> M (Name,[Type],Int,[(Name,Type)])
doLiftFun mbStore expr =
  do let (tps,pps,xs,body) = exprToFun expr
     (newTPs,newProps,newXs,(newBody,bodyT)) <-
          pushName mbStore $
          enterFun tps pps xs
          do -- traceM ("ENTER: " ++ maybe "?" (show . pp) mbStore)
             newB <- ll body
             t <- getExprType newB
             pure (newB,t)
     let allTPs   = newTPs   ++ tps
     let allProps = newProps ++ pps
     let allXs    = newXs    ++ xs
     f <- newName mbStore

     let newTy = Forall
                   { sVars  = allTPs
                   , sProps = allProps
                   , sType  = foldr (tFun . snd) bodyT allXs
                   }

     addDecl
       Decl { dName       = f
            , dSignature  = newTy
            , dDefinition = DExpr (mkLam allTPs allProps allXs newBody)
            , dPragmas    = []
            , dInfix      = False
            , dFixity     = Nothing
            , dDoc        = Nothing
            }

     let entry = (f, map (TVar . tpVar) newTPs, length newProps, newXs)
     case mbStore of
       Nothing -> pure ()
       Just key -> sets_ \rw -> rw { liftedVars = Map.insert key entry
                                                      (liftedVars rw) }
     pure entry

-- | Lift a declaration to the top level.
doLiftDecl :: Decl -> M ()
doLiftDecl d =
  case dDefinition d of
    DExpr e     -> void (doLiftFun (Just (dName d)) e)
    DForeign {} -> panic "doLiftDecl" ["Local foreign declaration"]
    DPrim {}    -> panic "doLiftDecl" ["Local primitive"]

isFunDecl :: Decl -> Bool
isFunDecl = isJust . tIsFun . sType . dSignature

instance LLMaybe Decl where
  llMaybe d
    | isFunDecl d = doLiftDecl d >> pure Nothing
    | otherwise =
      do def <- case dDefinition d of
                  DExpr e -> DExpr <$> ll e
                  de@DForeign {} -> pure de
                  de@DPrim {}    -> pure de
         pure (Just d { dDefinition = def })


-- XXX: This should be in Cryptol.IR.FreeVars
class Defs d where
  defs :: d -> Map Name Schema

instance Defs a => Defs [a] where
  defs = Map.unions . map defs

instance Defs DeclGroup where
  defs dg = case dg of
              Recursive ds   -> defs ds
              NonRecursive d -> defs d

instance Defs Decl where
  defs d = Map.singleton (dName d) (dSignature d)

instance Defs Match where
  defs m = case m of
             From x _ elT _ -> Map.singleton x (tMono elT)
             Let d -> defs d


