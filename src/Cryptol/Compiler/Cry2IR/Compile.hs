module Cryptol.Compiler.Cry2IR.Compile where

import Data.List(elemIndex)
import Data.Text qualified as Text
import Control.Monad(zipWithM,forM)

import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.Utils.RecordMap qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.Cry2IR.Monad qualified as S
import Cryptol.Compiler.Cry2IR.Specialize
import Cryptol.Compiler.Cry2IR.InstanceMap


compileModule :: Cry.Module -> M.CryC ()
compileModule m = mapM_ compileDeclGroup (Cry.mDecls m)

compileDeclGroup :: Cry.DeclGroup -> M.CryC ()
compileDeclGroup dg =
  case dg of
    Cry.NonRecursive d -> compileTopDecl d -- (toPrim d)
    Cry.Recursive ds   -> mapM_ (compileTopDecl . toPrim) ds
  where
  toPrim d = d { Cry.dDefinition = Cry.DPrim }

compileTopDecl :: Cry.Decl -> M.CryC ()
compileTopDecl d =
  debugWrap

  do insts <-
       case Cry.dDefinition d of
         Cry.DPrim       ->
           do insts <- compilePrimDecl (Cry.dSignature d)
              pure [ (i,ty,IRFunPrim) | (i,ty) <- insts ]
         Cry.DForeign {} -> M.unsupported "Foregin declaration" -- XXX: Revisit
         Cry.DExpr e ->
           do let (as,ps,xs,body) = prepExprDecl e
              M.withCryLocals xs    -- we add locals here so we can compute
                                    -- the type of the body
                do resTcry <- M.getTypeOf body
                   compileFunDecl as ps (map snd xs) resTcry \args resT ->
                      do let is = map (NameId . fst) xs
                         let nms = zipWith IRName is args
                         S.withIRLocals nms
                           do def <- compileExpr body resT
                              pure (IRFunDef is def)

     let isPrim def = case def of
                        IRFunPrim -> True
                        _         -> False
         hasPrims = not (null [ () | (_,_,p) <- insts, isPrim p ])
     funame <- compileFunName hasPrims cname

     let decls = map (mkDecl funame) insts
         mbMap = instanceMapFromList
                   [ (irfnInstance (irfName fd), fd) | fd <- decls ]
     case mbMap of
       Right a  -> M.addCompiled cname a
       Left err -> M.unsupported (Text.pack (show (pp cname)) <> ": " <> err)

  where
  cname = Cry.dName d

  mkDecl funame (i,ty,def) =
    IRFunDecl
      { irfName = IRFunName { irfnName = funame, irfnInstance = i }
      , irfType = ty
      , irfDef  = def
      }

  compileFunName pr nm =
    do mb <- M.isPrimDecl nm
       case mb of
         Just p | pr -> pure (IRPrimName (CryPrim p))
         _           -> pure (IRDeclaredFunName (NameId nm))

  debugWrap m =
    do mb <- M.catchError m
       case mb of
         Right _  -> pure ()
         Left err ->
          M.doIO
            do print (pp cname <+> ":" <+> cryPP (Cry.dSignature d))
               print (pp err)

-- | Identify expressions that are functions
prepExprDecl ::
  Cry.Expr -> ([Cry.TParam], [Cry.Prop], [(Cry.Name,Cry.Type)],Cry.Expr)
prepExprDecl expr = (tparams, quals, args, body)
  where
  (tparams,expr1) = Cry.splitWhile Cry.splitTAbs     expr
  (quals,expr2)   = Cry.splitWhile Cry.splitProofAbs expr1
  (args,body)     = Cry.splitWhile Cry.splitAbs      expr2



callPrim :: IRPrim -> [Expr] -> Type -> Expr
callPrim prim es tgtT =
  IRExpr $ IRCallFun
    IRCall
      { ircFun =
          IRTopFun
            IRTopFunCall
               { irtfName =
                   IRFunName
                    { irfnName     = IRPrimName prim
                    , irfnInstance = FunInstance []
                    }
               , irtfTypeArgs = []
               , irtfSizeArgs = []
               }
      , ircArgTypes = map typeOf es
      , ircResType = tgtT
      , ircArgs = es
      }

mkTuple :: [Expr] -> Expr
mkTuple es = callPrim Tuple es (TTuple (map typeOf es))


compileExpr :: Cry.Expr -> Type -> S.SpecM Expr
compileExpr expr0 tgtT =

  do let (args',expr1)            = Cry.splitWhile Cry.splitApp expr0
         args                     = reverse args'
         (expr2,tyArgs,_profApps) = Cry.splitExprInst expr1
         expr                     = Cry.dropLocs expr2
     case expr of

       Cry.EVar x -> compileVar x tyArgs args tgtT

       Cry.EList es t ->
         do it  <- compileValType t
            ces <- mapM (`compileExpr` it) es
            pure (callPrim MakeSeq ces tgtT)

       Cry.ETuple es ->
        case tgtT of
          TTuple ts -> mkTuple <$> zipWithM compileExpr es ts
          _  -> unexpected "ETuple of non-tuple type"

       Cry.ERec rec ->
          case tgtT of
            TTuple ts -> mkTuple <$>
                            zipWithM compileExpr (Cry.recordElements rec) ts

            _ -> unexpected "Record at non-tuple type"

       Cry.ESel e sel ->
         case sel of
           Cry.TupleSel n _   -> doTuple (Left n)
           Cry.RecordSel nm _ -> doTuple (Right nm)
           Cry.ListSel _n _   -> S.unsupported "XXX: ListSel"

         where
         doTuple n =
            do cty <- S.doCryC (M.getTypeOf e)
               ty  <- compileValType cty
               let i = case n of
                         Left j -> j
                         Right nm ->
                           case Cry.tNoUser cty of
                             Cry.TRec fs ->
                               case elemIndex nm $
                                        map fst (Cry.canonicalFields fs) of
                                 Just j -> j
                                 Nothing -> unexpected' [ "Missing field"
                                                        , show (cryPP nm)
                                                        ]
                             _ -> unexpected' [ "Bad record selector"
                                              , show (cryPP cty)
                                              ]

               (_resT,len) <-
                  case ty of
                    TTuple ts | t : _ <- drop i ts -> pure (t, length ts)
                    typ -> unexpected'
                             [ "Bad argument of tuple selector"
                             , show (pp typ)
                             ]
               -- XXX: check that resT matches TgtT?
               ce <- compileExpr e ty
               pure $ callPrim (TupleSel i len) [ce] tgtT

       Cry.ESet {} -> S.unsupported "ESet"

       Cry.EIf eCond eThen eElse ->
         do ceCond <- compileExpr eCond TBool
            ceThen <- compileExpr eThen tgtT
            ceElse <- compileExpr eElse tgtT
            pure (IRExpr (IRIf ceCond ceThen ceElse))

       Cry.EComp len ty res ms ->
         do sz   <- compileStreamSizeType len
            elTy <- compileValType ty
            compileComprehension sz elTy tgtT res ms

       Cry.EWhere e ds -> compileLocalDeclGroups ds (compileExpr e tgtT)

       Cry.EPropGuards {}        -> S.unsupported "EPropGuards"

       Cry.ELocated _rng e       -> compileExpr e tgtT

       Cry.EAbs {} -> compileLam xs' e args tgtT
          where (xs',e) = Cry.splitWhile Cry.splitAbs expr
       Cry.EApp {}               -> unexpected "EApp"
       Cry.ETAbs {}              -> unexpected "ETAbs"
       Cry.ETApp {}              -> unexpected "ETApp"
       Cry.EProofAbs {}          -> unexpected "EProofAbs"
       Cry.EProofApp {}          -> unexpected "EProofApp"


  where
  unexpected msg = unexpected' [msg]
  unexpected' msg = panic "compileExpr" msg


compileLam ::
  [(Cry.Name, Cry.Type)] -> Cry.Expr -> [Cry.Expr] -> Type -> S.SpecM Expr
compileLam xs' e' args tgtT = foldr addDef doFun defs
  where
  defs    = zip xs' args
  xs      = drop (length defs) xs'

  addDef ((x,t),e) k =
    do ty  <- compileValType t
       ec  <- compileExpr e ty
       let nm = IRName (NameId x) ty
       ek <- S.withLocals [(nm,t)] k
       pure (IRExpr (IRLet nm ec ek))

  doFun
    | null xs = compileExpr e' tgtT
    | otherwise =
      do params <- forM xs \(x,t) -> IRName (NameId x) <$> compileValType t
         resT'  <- S.zonk tgtT
         case resT' of
           TFun as b ->
              let have = length params
                  need = length as
                  nameTs = zip params (map snd xs)
              in case compare have need of
                   EQ -> do body <- S.withLocals nameTs (compileExpr e' b)
                            pure (IRExpr (IRLam params body))

                   LT -> S.unsupported "Lambda arity mismatch: LT" 
                   -- eta expand
                   -- (\x -> e) :: (Int,Int) -> Int
                   -- |x,y| e y

                   GT -> S.unsupported "Lambda arity mismatch: LT" 
                   -- (\x y -> e) -> Int -> (Int -> Int)
                   -- \x -> \y -> e

           _ -> unexpected "Lambda but result is not function"

  unexpected msg = panic "compileLam" [msg]


{- Compiling comprehensions:

Single arm, single match: [ e | x <- e1 ]

    e1.iter.map(|x| e)

Single arm, multiple matches: [ e | x <- e1, y <- e2 ]

    e1.iter.flatMap(|x| e2.iter.map(|y| e))

Multiple arms, single match:  [ e | x <- e1 | y <- e2 ]

    zip(e1.iter,e2.iter).map(|x,y| e)

Multiple arms, multiple matches:
  [ e | x <- e1, y <- e2, z <- e3
      | a <- e4
  ]

  zip( e1.iter.flatMap(|x| e2.iter.flatMap(|y| e3.iter.map(|z| (x,y,z))))
     , e4.iter
     ).map(|(x,y,z),a| e)

-}

compileComprehension ::
  StreamSize -> Type -> Type -> Cry.Expr -> [[Cry.Match]] -> S.SpecM Expr
compileComprehension _sz elT tgtT res mss =
  (`coerceTo` tgtT) =<<

  case mss of
    []   ->  unexpected "Emppty zip"

    [ms] -> doOneAltArm ms

    _ ->
      do arms <- mapM doZipArm mss

         let jnGen (aNames, a) (bNames, b) =
                let (la,ta)       = case typeOf a of
                                      TStream p q -> (p,q)
                                      _ -> unexpected "Not TStream"
                    (lb,tb)       = case typeOf b of
                                      TStream p q -> (p,q)
                                      _ -> unexpected "Not TStream"
                    len           = evalSizeType Cry.TCMin [la,lb]
                    sel (x,_) n e = callPrim (TupleSel n 2) [e] (typeOf x)

                in ( [ (x, f . sel x 0) | (x,f) <- aNames ] ++
                     [ (x, f . sel x 1) | (x,f) <- bNames ]
                   , callPrim Zip [ a, b ] (TStream len (TTuple [ta,tb]))
                   )

         let (nms,gen) = foldr1 jnGen arms

         let (genLen,tupTy) =
                case typeOf gen of
                  TStream l t -> (l,t)
                  _ -> unexpected "Generator not TStream"

         tupName <- (`IRName` tupTy) <$> S.doCryC M.newNameId
         let tupExpr = IRExpr (IRVar tupName)
             defName ((x,t),f) k =
               IRExpr . IRLet x (f tupExpr) <$> S.withLocals [(x,t)] k

         body <- IRExpr . IRLam [tupName] <$>
                 foldr defName (compileExpr res elT) nms 

         pure (callPrim Map [gen,body] (TStream genLen elT))

  where
  doOneAltArm ms =
    case ms of
      [] -> unexpected "OneAlt: empty arm"

      m : more ->
        do (name,ty,lenTy,it) <- doMatch m
           S.withLocals [(name,ty)]
             case more of
               [] ->
                 do body <- compileExpr res elT
                    let fun = IRExpr (IRLam [name] body)
                    pure (callPrim Map [it,fun] (TStream lenTy elT))
               _ ->
                 do body <- doOneAltArm ms
                    let fun    = IRExpr (IRLam [name] body)
                        newLen =
                          case typeOf body of
                            TStream l _ -> evalSizeType Cry.TCMul [lenTy,l]
                            _ -> unexpected "rest not TStream"
                    pure (callPrim FlatMap [it,fun] (TStream newLen elT))

  doZipArm ms =
    case ms of
      []  -> unexpected "Zip: empty arm"
      [m] ->
        do (name,ty,_lenTy,it) <- doMatch m
           pure ([((name,ty),id)],it)
      _   -> doMultiZipArm [] ms

  doMultiZipArm nms ms =
    case ms of
      [] -> unexpected "MultiZip: empty arm"
      m : more ->
        do (name,ty,lenTy,it) <- doMatch m
           S.withLocals [(name,ty)]
             case more of
               [] ->
                 do let allNms    = reverse ((name,ty) : nms)

                        len       = length allNms
                        tupT      = TTuple (map (typeOf . fst) allNms)
                        tupE      = mkTuple (map (IRExpr . IRVar . fst) allNms)

                        sel x n e = callPrim (TupleSel n len) [e] (typeOf x)
                        ns = [ ((x,t),sel x fi) |((x,t),fi) <- zip allNms [0..]]
                        fu = IRExpr (IRLam [name] tupE)
                        newIt = callPrim Map [it,fu] (TStream lenTy tupT)
                    pure (ns,newIt)

               _ ->
                 do (allNs,body) <- doMultiZipArm ((name,ty) : nms) more
                    let fu = IRExpr (IRLam [name] body)
                        newL =
                          case typeOf body of
                            TStream l _ -> evalSizeType Cry.TCMul [lenTy,l]
                            _ -> unexpected "rest not TStream"
                        newIt = callPrim FlatMap [it,fu] (TStream newL elT)
                    pure (allNs,newIt)

  doMatch m =
    case m of
      Cry.From x len ty gen ->
        do lenTy   <- compileStreamSizeType len
           locElTy <- compileValType ty
           it      <- compileExpr gen (TStream lenTy locElTy)
           let name = IRName (NameId x) locElTy
           pure (name,ty,lenTy,it)
      Cry.Let {} -> S.unsupported "XXX: Let in EComp"

  unexpected msg = panic "compileComprehension" [msg]


compileLocalDeclGroups :: [Cry.DeclGroup] -> S.SpecM Expr -> S.SpecM Expr
compileLocalDeclGroups dgs k =
  case dgs of
    [] -> k
    d : ds -> compileLocalDeclGroup d (compileLocalDeclGroups ds k)

compileLocalDeclGroup :: Cry.DeclGroup -> S.SpecM Expr -> S.SpecM Expr
compileLocalDeclGroup dg k =
  case dg of
    Cry.Recursive {} -> S.unsupported "recursive local declaration" -- XXX
    Cry.NonRecursive d ->
      do let schema = Cry.dSignature d
         case (Cry.sVars schema, Cry.sProps schema) of
           ([],[]) ->
              do let cty = Cry.sType schema
                 ty <- compileValType (Cry.sType schema)
                 case Cry.dDefinition d of
                   Cry.DExpr e ->
                     do e' <- compileExpr e ty
                        let cname = Cry.dName d
                            name  = IRName (NameId cname) ty
                        ek <- S.withLocals [(name, cty)] k
                        pure (IRExpr (IRLet name e' ek))
                   Cry.DPrim {}    -> unexpected "Local primitve"
                   Cry.DForeign {} -> unexpected "Local foreign declaration"
           _ -> S.unsupported "Polymorphic local variable"
  where
  unexpected msg = panic "compileLocalDeclGroup" [msg]


compileVar :: Cry.Name -> [Cry.Type] -> [Cry.Expr] -> Type -> S.SpecM Expr
compileVar x ts args tgtT =
  do mb <- S.getLocal (NameId x)
     case mb of
       Nothing -> compileCall x ts args tgtT
       Just n ->
         case (ts,args) of
           ([], []) -> coerceTo (IRExpr (IRVar n)) tgtT
           ([], es) ->
             do ty <- S.zonk (typeOf n)
                case ty of
                  TFun as b ->
                    do let have = length es
                           need = length as
                           (haveTs,needTs) = splitAt have as
                       ces <- zipWithM compileExpr es as
                       let call = IRCall
                                    { ircFun  = IRFunVal (IRExpr (IRVar n))
                                    , ircArgTypes = haveTs
                                    , ircResType = b
                                    , ircArgs = ces
                                    }
                       expr <- case compare have need of
                                 EQ -> pure (IRCallFun call)
                                 LT -> pure (IRClosure call
                                               { ircResType = TFun needTs b })
                                 GT -> S.unsupported "over application"
                       coerceTo (IRExpr expr) tgtT

                  _ -> unexpected "application to non-function"
           (_ : _, _) -> S.unsupported "Polymorphic locals"
  where
  unexpected msg = panic "compileVar" [msg]


-- | Compile a all to a function.
compileCall ::
  Cry.Name    {- ^ Function name -} ->
  [Cry.Type]  {- ^ Type arguments at instantiation -} ->
  [Cry.Expr]  {- ^ Value arguments provided -} ->
  Type        {- ^ Wanted result type -} ->
  S.SpecM Expr
compileCall f ts es tgtT =
  do instDB <- S.doCryC (M.getFun f)
     tys    <- mapM compileType ts
     (funName,funTy) <-
        case lookupInstance tys instDB of
          ITE gs opt1 opt2 -> doITE gs opt1 opt2
          Found a -> pure a
          NotFound ->
            bad
              [ "Missing instance"
              , "Function: " ++ show (pp f)
              , "Instance: " ++ show [ either pp pp x | x <- tys ]
              ]
     let (typeArgs',sizeArgs') = makeTArgs [] [] (irfnInstance funName) tys
     typeArgs <- mapM S.zonk typeArgs'
     sizeArgs <- forM sizeArgs' \(t',s) ->
                   do t <- S.zonk t'
                      pure (t,s)

     let sizeSu = foldr (uncurry suAddSize) suEmpty
                $ zip (map irsName (ftSizeParams funTy))
                      (map (IRSize . fst) sizeArgs)

         su = foldr (uncurry suAddType) sizeSu
            $ zip (ftTypeParams funTy) typeArgs

         argTs = apSubst su (ftParams funTy)
         resT  = apSubst su (ftResult funTy)

     let haveArgs = length es
         needArgs = length argTs
         (haveTs,needTs) = splitAt haveArgs argTs

     ces <- zipWithM compileExpr es argTs
     let call = IRCall { ircFun      = IRTopFun
                                         IRTopFunCall
                                           { irtfName = funName
                                           , irtfTypeArgs = typeArgs
                                           , irtfSizeArgs = sizeArgs
                                           }
                       , ircArgTypes  = haveTs
                       , ircResType   = resT
                       , ircArgs      = ces
                       }


     res <- case compare haveArgs needArgs of
              EQ -> pure (IRCallFun call)
              LT -> pure (IRClosure call { ircResType = TFun needTs resT })
              GT -> S.unsupported "function over applied (higher order result)"

     coerceTo (IRExpr res) tgtT

  where
  bindNum sz ty args =
    case ty of
      Right e' ->
        case e' of
          IRSize e  -> (e,sz) : args
          IRInfSize -> bad [ "Inf argument, to finite instnace" ]
      Left {} -> bad [ "Type argument mismatch"
                     , "Expected Num, got Type"
                     ]
  bindType ty args =
    case ty of
      Left t   -> t : args
      Right {} -> bad [ "Type argument mismatch"
                      , "Expected Type, got Num"
                      ]

  makeTArgs typeArgs sizeArgs (FunInstance ps) cts =
    case (ps,cts) of
      (p : morePs, t : moreTs) ->
        let next a b = makeTArgs a b (FunInstance morePs) moreTs
        in
        case p of
          NumVar sz   -> next typeArgs (bindNum sz t sizeArgs)
          NumFixed {} -> next typeArgs sizeArgs
          TyBool      -> next typeArgs sizeArgs
          TyNotBool   -> next (bindType t typeArgs) sizeArgs
          TyAny       -> next (bindType t typeArgs) sizeArgs

      ([],[]) -> (reverse typeArgs, reverse sizeArgs)
      _ -> bad ["Instance and type parameters do not match"]

  doITE gs opt1 opt2 =
    case gs of
      [] -> doRes opt1
      g : more ->
        case g of
          GBool x ->
            do yes <- S.caseBool x
               if yes then doITE more opt1 opt2 else doRes opt2
          GNotBool x ->
            do yes <- S.caseBool x
               if yes then doRes opt2 else doITE more opt1 opt2

          -- can consider dynamic check if all alternatives return
          -- the same type
          GNum x n ->
            do yes <- S.caseConst (toCryS (IRPolySize x)) EQ n
               if yes then doITE more opt1 opt2 else doRes opt2
          GNumFun tf as rel n ->
            do yes <- S.caseConst (toCryS (IRComputedSize tf as)) rel n
               if yes then doITE more opt1 opt2 else doRes opt2

  toCryIS sz =
    case sz of
      IRInfSize -> Cry.tInf
      IRSize n  -> toCryS n

  toCryS sz =
    case sz of
      IRFixedSize n         -> Cry.tNum n
      IRPolySize x          -> Cry.TVar (Cry.TVBound (irsName x))
      IRComputedSize tf as  -> Cry.TCon (Cry.TF tf) (map toCryIS as)

  doRes res =
    case res of
      Found a  -> pure a
      NotFound -> bad ["NotFound"]
      ITE gs opt1 opt2 -> doITE gs opt1 opt2

  bad = panic "compileCall"


compileType :: Cry.Type -> S.SpecM (Either Type StreamSize)
compileType ty =
  case Cry.kindOf ty of
    Cry.KNum  -> Right <$> compileStreamSizeType ty
    Cry.KType -> Left  <$> compileValType ty
    _         -> S.panic "compileTypes" ["Unexpecte higher order kind"]



coerceTo :: Expr -> Type -> S.SpecM Expr
coerceTo e tgtT' =
  do srcT <- S.zonk (typeOf e)
     tgtT <- S.zonk tgtT'

     case (srcT,tgtT) of

       (TStream (IRSize l1) t1, TArray l2 t2)
         | (l1,t1) == (l2,t2) -> pure (callPrim Collect [e] tgtT)

       (TStream (IRSize l1) TBool, TWord l2)
          | l1 == l2          -> pure (callPrim Collect [e] tgtT)

       (TArray l1 t1, TStream (IRSize l2) t2)
         | (l1,t1) == (l2,t2) -> pure (callPrim Iter [e] tgtT)

       (TWord l1, TStream (IRSize l2) TBool)
         | l1 == l2           -> pure (callPrim Iter [e] tgtT)

       _ | srcT == tgtT -> pure e
         | otherwise -> panic "adjustType"
                          [ "Cannot coerce types"
                          , "From: " ++ show (pp srcT)
                          , "To  : " ++ show (pp tgtT)
                          ]









