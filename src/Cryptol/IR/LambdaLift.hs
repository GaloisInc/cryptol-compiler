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
module Cryptol.IR.LambdaLift where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(isJust,catMaybes)

import MonadLib

import Cryptol.Utils.Panic(panic)
import Cryptol.Utils.RecordMap
import Cryptol.Utils.Ident(packIdent)
import Cryptol.Parser.Position(emptyRange)
import Cryptol.TypeCheck.AST
import Cryptol.TypeCheck.TypeOf(fastTypeOf)
import Cryptol.ModuleSystem.Name(mkDeclared,Supply,nameInfo,NameInfo(..)
                                , NameSource(SystemName)
                                , ModPath, Namespace(NSValue))
import Cryptol.IR.FreeVars(freeVars,Deps(..))


type M    = ReaderT RO (StateT RW (ExceptionT Err Id))

type Err  = String

data RO = RO
  { modPath         :: ModPath
    -- ^ Current ModPath, used when we make new names for the lifted functions.

  , topVars         :: Map Name Schema
    -- ^ Types of top-level declaraitions, so we can compute types of things.

  , outerVars       :: Map Name Schema
    -- ^ Locals in outer functions.

  , outerConstraints :: [Prop]
    -- ^ Constraints in outer functions.

  , localTParams  :: Set TParam
    -- ^ Type parameters for the current function.
    -- We don't need to capture these type variables.

  , localConstraints :: [Prop]
    -- ^ Constraints on local function


  , localVars     :: Map Name Schema
    -- ^ Locals defined in the current functions.
    -- These ones we don't need to capture.

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

newName :: M Name
newName =
  do mp <- modPath <$> ask
     sets \rw ->
       case mkDeclared NSValue mp SystemName i Nothing r (nameSupply rw) of
         (x, newSup) -> (x, rw { nameSupply = newSup })
  where
  i = packIdent "lambda_lift"
  r = emptyRange


-- | Since we don't suport recrusive functions at the momennt,
-- we just emit a non-recursive component. This would have to change
-- a bit when we fix up the recursive function support.
addDecl :: Decl -> M ()
addDecl d = sets_ \rw -> rw { newDecls = NonRecursive d : newDecls rw }


withLocals :: Map Name Schema -> M a -> M a
withLocals xs =
  mapReader \ro -> ro { localVars = Map.union xs (localVars ro) }

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
     sets \rw ->
       let newTPs = extraTParams rw
           newProps = [] -- XXX:!!!!
           newPs  = extraParams rw
       in ( ( Set.toList newTPs
            , newProps
            , Map.toList  newPs
            , a
            )

          , rw { extraTParams = newTPs <> outTPs
               , extraParams  = newPs  <> outPs
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
useVar x t =
  do outer   <- outerVars <$> ask
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
     pure (fastTypeOf env e)


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

instance LL a => LL [a] where
  ll = traverse ll

instance (LL a, LL b) => LL (a,b) where
  ll (a,b) = (,) <$> ll a <*> ll b

instance LL a => LL (RecordMap k a) where
  ll = traverse ll

instance LL Selector where
  ll = pure

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
          case ds of
            [] -> x
            _  -> EWhere x newDs

    where
    doFun = callLambda expr [] 0 []

    doCall =
      do let (fun, tyArgs, proofArgs, args) = exprToCall expr
         mapM_ ll tyArgs
         newArgs <- mapM ll args

         ty <- getExprType fun
         let needCall = not (null tyArgs)
                     || proofArgs > 0
                     || isJust (tIsFun ty)

         case dropLocs fun of

           -- Variable
           EVar x ->
             do useVar x ty
                if needCall
                  then
                    do liftMap <- liftedVars <$> get
                       case Map.lookup x liftMap of
                         Just (topFun,moreTs,morePs,moreXs) ->
                           do mapM_ ll moreTs
                              mapM_ (uncurry useVar) moreXs
                              pure (mkCall (EVar topFun)
                                           (moreTs ++ tyArgs)
                                           (morePs + proofArgs)
                                           (map (EVar . fst) moreXs ++ newArgs))


                         Nothing -> pure expr
                  else pure expr

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
          else Just . Recursive . catMaybes <$> mapM llMaybe ds
                                           {- These should all be Just! -}
doLiftFun :: Maybe Name -> Expr -> M (Name,[Type],Int,[(Name,Type)])
doLiftFun mbStore expr =
  do let (tps,pps,xs,body) = exprToFun expr
     (newTPs,newProps,newXs,newBody) <- enterFun tps pps xs (ll body)
      -- XXX: should we generate fresh names for the type and value parameters?
     let allTPs   = newTPs ++ tps
     let allProps = newProps ++ pps
     let allXs    = newXs ++ xs
     f <- newName

     bodyT <- getExprType newBody

     let newTy = Forall
                   { sVars  = allTPs
                   , sProps = allProps
                   , sType  = foldr (tFun . snd) bodyT allXs
                   }

     let withArgs     = foldr (uncurry EAbs) newBody allXs
         withProofs   = foldr EProofAbs withArgs allProps
         newDef       = foldr ETAbs withProofs allTPs

     addDecl
       Decl { dName       = f
            , dSignature  = newTy
            , dDefinition = DExpr newDef
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


