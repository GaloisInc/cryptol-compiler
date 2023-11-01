{-# Language LambdaCase #-}
module Cryptol.Compiler.Cry2IR.Compile where

import Data.Set qualified as Set
import Data.List(elemIndex)
import Data.Either(partitionEithers)
import Data.Text(Text)
import Control.Monad(zipWithM,forM,guard,when,unless)

import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.TypeCheck.Subst qualified as Cry
import Cryptol.IR.FreeVars qualified as Cry
import Cryptol.Utils.RecordMap qualified as Cry
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Common
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.Cry2IR.Monad qualified as S
import Cryptol.Compiler.Cry2IR.ConvertM qualified as C
import Cryptol.Compiler.Cry2IR.Specialize qualified as Spec
import Cryptol.Compiler.Cry2IR.Type qualified as T
import Cryptol.Compiler.Cry2IR.InstanceMap


compileModule :: Cry.Module -> M.CryC ()
compileModule m = mapM_ compileDeclGroup (Cry.mDecls m)

compileDeclGroup :: Cry.DeclGroup -> M.CryC ()
compileDeclGroup dg =
  case dg of
    Cry.NonRecursive d -> compileTopDecl d
    Cry.Recursive ds   -> mapM_ (compileTopDecl . toPrim) ds -- XXX
  where
  toPrim d = d { Cry.dDefinition = Cry.DPrim }

compileTopDecl :: Cry.Decl -> M.CryC ()
compileTopDecl d =
  debugWrap

  do insts <-
       case Cry.dDefinition d of
         Cry.DPrim       ->
           do insts <- Spec.compilePrimDecl cname (Cry.dSignature d)
              pure [ (i,ty,IRFunPrim) | (i,ty) <- insts ]
         Cry.DForeign {} -> M.unsupported loc "Foregin declaration" -- XXX: Revisit
         Cry.DExpr e ->
           do insts <- Spec.compilePrimDecl cname (Cry.dSignature d)
              pure [ (i,ty,IRFunPrim) | (i,ty) <- insts ]
{-
           do let (as,ps,xs,body) = prepExprDecl e
              M.withCryLocals xs    -- we add locals here so we can compute
                                    -- the type of the body
                do resTcry <- M.getTypeOf body
                   insts <- compileFunDecl cname as ps (map snd xs) resTcry \args resT ->
                      pure () {-
                      do let is = map (NameId . fst) xs
                         let nms = zipWith IRName is args
                         C.withIRLocals nms
                           do def <- compileExpr body resT
                              pure (IRFunDef is def) -}
                   pure [ (i,ty,IRFunPrim) | (i,ty,_) <- insts ]
-}

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
       Left err -> M.unsupported loc err

  where
  loc   = [pp cname]
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




makeInstance ::
  Cry.Name ->
  (FunInstance, FunType) {- ^ This is what we want -} ->
  ([Cry.TParam], [Cry.Prop], [(Cry.Name,Cry.Type)], Cry.Expr) {- ^ Def -} ->
  M.CryC FunDef
makeInstance f (i@(FunInstance info), funTy) (tps, _, argsG, bodyG) =
  do expr <-
        C.runConvertM $
        C.enterFun f i $
        C.withNumericTParams (ftSizeParams funTy) $
        C.withLocals args $
        compileExpr body (ftResult funTy)
     pure (IRFunDef (map (irNameName . fst) args) expr)
  where
  args = [ (IRName (NameId x) irt, Cry.apSubst crySu t)
         | ((x,t),irt) <- argsG `zip` ftParams funTy ]
  body = Cry.apSubst crySu bodyG

  crySu = Cry.listParamSubst (foldr isKnown [] (zip info tps))
  isKnown (pi,tp) rest =
    case pi of
      NumFixed n -> (tp, Cry.tNat' n) : rest
      TyBool     -> (tp, Cry.tBit) : rest
      _          -> rest






-- | Identify expressions that are functions
prepExprDecl ::
  Cry.Expr -> ([Cry.TParam], [Cry.Prop], [(Cry.Name,Cry.Type)],Cry.Expr)
prepExprDecl expr = (tparams, quals, args, body)
  where
  (tparams,expr1) = Cry.splitWhile Cry.splitTAbs     expr
  (quals,expr2)   = Cry.splitWhile Cry.splitProofAbs expr1
  (args,body)     = Cry.splitWhile Cry.splitAbs      expr2


seqElementType :: Type -> Type
seqElementType ty =
  case ty of
    TWord _     -> TBool
    TArray _ t  -> t
    TStream _ t -> t
    _           -> panic "seqElementType" ["Not a sequence"]

seqLength :: Type -> StreamSize
seqLength ty =
  case ty of
    TWord l     -> IRSize l
    TArray l _  -> IRSize l
    TStream l _ -> l
    _           -> panic "seqLength" ["Not a sequence"]

compileExpr :: Cry.Expr -> Type -> C.ConvertM Expr
compileExpr expr0 tgtT =

  do let (args',expr1)            = Cry.splitWhile Cry.splitApp expr0
         args                     = reverse args'
         (expr2,tyArgs,_profApps) = Cry.splitExprInst expr1
         expr                     = Cry.dropLocs expr2
     case expr of

       Cry.EVar x -> compileVar x tyArgs args tgtT

       Cry.EList es _t ->
         do let newTgtT = seqElementType tgtT
            ces <- mapM (`compileExpr` newTgtT) es
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
           Cry.ListSel i mb ->
             do let i' = toInteger i
                when (i' > maxSizeVal)
                     (C.unsupported "List selector index is too large.")
                len <- case mb of
                         Nothing -> unexpected "Missing length in ListSel"
                         Just l  -> pure (IRFixedSize (toInteger l))
                let ety = case tgtT of
                            TBool -> TWord len
                            _     -> TArray len tgtT
                ce <- compileExpr e ety
                pure (callPrimG ArrayLookup {- or word? -}
                          [(IRFixedSize i',MemSize)] [ce] tgtT)

         where
         doTuple n =
            do cty <- C.doCryC (M.getTypeOf e)
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

               (ty,len) <-
                  do t <- T.compileValType cty
                     case t of
                       TTuple ts
                         | (as,_:bs) <- splitAt i ts ->
                           pure (TTuple (as ++ tgtT:bs), length ts)

                       typ -> unexpected'
                                    [ "Bad argument of tuple selector"
                                    , show (pp typ)
                                    ]

               ce <- compileExpr e ty
               pure $ callPrim (TupleSel i len) [ce] tgtT


       Cry.ESet {} -> C.unsupported "ESet"

       Cry.EIf eCond eThen eElse ->
         do ceCond <- compileExpr eCond TBool
            ceThen <- compileExpr eThen tgtT
            ceElse <- compileExpr eElse tgtT
            pure (IRExpr (IRIf ceCond ceThen ceElse))

       Cry.EComp len ty res ms ->
         let elT = seqElementType tgtT
         in compileComprehension len elT tgtT res ms

       Cry.EWhere e ds -> compileLocalDeclGroups ds (compileExpr e tgtT)

       Cry.EPropGuards {}        -> C.unsupported "EPropGuards"

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
  unexpected'    = panic "compileExpr"


compileLam ::
  [(Cry.Name, Cry.Type)] -> Cry.Expr -> [Cry.Expr] -> Type -> C.ConvertM Expr
compileLam xs' e' args tgtT = foldr addDef doFun defs
  where
  defs    = zip xs' args
  xs      = drop (length defs) xs'

  addDef ((x,t),e) k =
    do ty  <- T.compileValType t
       ec  <- C.enterLocal x (compileExpr e ty)
       let nm = IRName (NameId x) ty
       ek <- C.withLocals [(nm,t)] k
       pure (IRExpr (IRLet nm ec ek))

  doFun
    | null xs = compileExpr e' tgtT
    | otherwise =
      do params <- forM xs \(x,t) -> IRName (NameId x) <$> T.compileValType t
         case tgtT of
           TFun as b ->
              let have = length params
                  need = length as
                  nameTs = zip params (map snd xs)
              in case compare have need of
                   EQ -> do body <- C.withLocals nameTs (compileExpr e' b)
                            pure (IRExpr (IRLam params body))

                   LT -> C.unsupported "Lambda arity mismatch: LT" 
                   -- eta expand
                   -- (\x -> e) :: (Int,Int) -> Int
                   -- |x,y| e y

                   GT -> C.unsupported "Lambda arity mismatch: LT" 
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
  Cry.Type -> Type -> Type -> Cry.Expr -> [[Cry.Match]] -> C.ConvertM Expr
compileComprehension cryTotLen elT tgtT res mss =
  do e <- comp
            case seqLength tgtT of
              IRInfSize -> True
              _         -> False
     e `coerceTo` tgtT
  where
  comp isInf =
    case mss of
      []   ->  unexpected "Emppty zip"

      [ms] -> doOneAltArm isInf ms

      _ ->
        do arms <- mapM (doZipArm isInf) mss

           let jnGen (aNames, a) (bNames, b) =
                  let (la,ta)       = case typeOf a of
                                        TStream p q -> (p,q)
                                        _ -> unexpected "Not TStream"
                      (lb,tb)       = case typeOf b of
                                        TStream p q -> (p,q)
                                        _ -> unexpected "Not TStream"
                      len           = evalSizeMin la lb
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

           tupName <- (`IRName` tupTy) <$> C.doCryC M.newNameId
           let tupExpr = IRExpr (IRVar tupName)
               defName ((x,t),f) k =
                 IRExpr . IRLet x (f tupExpr) <$> C.withLocals [(x,t)] k

           body <- IRExpr . IRLam [tupName] <$>
                   foldr defName (compileExpr res elT) nms

           pure (callPrim Map [gen,body] (TStream genLen elT))


  -- NOTE: It is an invariant on the type of list comprehensions that
  -- only the first of sequnces of generators may be infinite, all others
  -- should be finite.
  genMulLen isInf this bodyRest
    | isInf     = IRInfSize
    | otherwise =
      case typeOf bodyRest of
        TStream rest _ -> IRSize (evalSizeType Cry.TCMul [this,rest])
        _              -> unexpected "rest not TStream"

  doOneAltArm isInf ms =
    case ms of
      [] -> unexpected "OneAlt: empty arm"

      m : more ->
        do (name,ty,lenTy,it) <- doMatch m
           C.withLocals [(name,ty)]
             case more of
               [] ->
                 do body <- compileExpr res elT
                    let fun = IRExpr (IRLam [name] body)
                    pure (callPrim Map [it,fun] (TStream lenTy elT))
               _ ->
                 do body <- doOneAltArm False more
                    let fun    = IRExpr (IRLam [name] body)
                        newLen = genMulLen isInf lenTy body
                    pure (callPrim FlatMap [it,fun] (TStream newLen elT))

  doZipArm isInfTot ms =
    case ms of
      []  -> unexpected "Zip: empty arm"
      [m] ->
        do (name,ty,_lenTy,it) <- doMatch m
           pure ([((name,ty),id)],it)
      _   -> doMultiZipArm isInfTot [] ms

  doMultiZipArm isInf nms ms =
    case ms of
      [] -> unexpected "MultiZip: empty arm"
      m : more ->
        do (name,ty,lenTy,it) <- doMatch m
           C.withLocals [(name,ty)]
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
                 do (allNs,body) <- doMultiZipArm False ((name,ty) : nms) more
                    let fu    = IRExpr (IRLam [name] body)
                        newL  = genMulLen isInf lenTy body
                        newIt = callPrim FlatMap [it,fu] (TStream newL elT)
                    pure (allNs,newIt)

  doMatch m =
    case m of
      Cry.From x len ty gen ->
        do lenTy   <- T.compileStreamSizeType len
           locElTy <- T.compileValType ty
           it      <- compileExpr gen (TStream lenTy locElTy)
           let name = IRName (NameId x) locElTy
           pure (name,ty,lenTy,it)
      Cry.Let {} -> C.unsupported "XXX: Let in EComp"

  unexpected msg = panic "compileComprehension" [msg]


compileLocalDeclGroups :: [Cry.DeclGroup] -> C.ConvertM Expr -> C.ConvertM Expr
compileLocalDeclGroups dgs k =
  case dgs of
    [] -> k
    d : ds -> compileLocalDeclGroup d (compileLocalDeclGroups ds k)

compileLocalDeclGroup :: Cry.DeclGroup -> C.ConvertM Expr -> C.ConvertM Expr
compileLocalDeclGroup dg k =
  case dg of
    Cry.Recursive ds ->
      case isRecValueGroup ds of
        Right yes -> compileRecursiveStreams yes k
        Left prob -> C.unsupported $ vcat [ "recursive local declaration"
                                          , prob
                                          ]
    Cry.NonRecursive d ->
      do let schema = Cry.dSignature d
         case (Cry.sVars schema, Cry.sProps schema) of
           ([],[]) ->
              do let cty = Cry.sType schema
                 ty <- T.compileValType (Cry.sType schema)
                 case Cry.dDefinition d of
                   Cry.DExpr e ->
                     do e' <- compileExpr e ty
                        let cname = Cry.dName d
                            name  = IRName (NameId cname) ty
                        ek <- C.withLocals [(name, cty)] k
                        pure (IRExpr (IRLet name e' ek))
                   Cry.DPrim {}    -> unexpected "Local primitve"
                   Cry.DForeign {} -> unexpected "Local foreign declaration"
           _ -> C.unsupported "Polymorphic local variable"
  where
  unexpected msg = panic "compileLocalDeclGroup" [msg]

compileVar :: Cry.Name -> [Cry.Type] -> [Cry.Expr] -> Type -> C.ConvertM Expr
compileVar x ts args tgtT =
  do mb <- C.getLocal (NameId x)
     case mb of
       Nothing -> compileCall x ts args tgtT
       Just n ->
         case (ts,args) of
           ([], []) -> coerceTo (IRExpr (IRVar n)) tgtT   -- local mono value

           -- local mono function
           ([], es) ->
             do let ty = typeOf n
                case ty of
                  TFun as b ->
                    do let have = length es
                           need = length as
                           (haveTs,needTs) = splitAt have as
                       ces <- zipWithM compileExpr es as
                       let call = IRCall
                                    { ircFun  = IRFunVal (IRExpr (IRVar n))
                                    , ircArgTypes = haveTs
                                    , ircFunType = monoFunType haveTs b
                                    , ircResType = b
                                    , ircArgs = ces
                                    }
                       expr <- case compare have need of
                                 EQ -> pure (IRCallFun call)
                                 LT -> pure (IRClosure call
                                               { ircResType = TFun needTs b })
                                 GT -> C.unsupported "over application"
                       coerceTo (IRExpr expr) tgtT

                  _ -> unexpected "application to non-function"
           (_ : _, _) -> C.unsupported "Polymorphic locals"
  where
  unexpected msg = panic "compileVar" [msg]


-- | Compile a all to a function.
compileCall ::
  Cry.Name    {- ^ Function name -} ->
  [Cry.Type]  {- ^ Type arguments at instantiation -} ->
  [Cry.Expr]  {- ^ Value arguments provided -} ->
  Type        {- ^ Wanted result type -} ->
  C.ConvertM Expr
compileCall f ts es tgtT = undefined
  do instDB <- C.doCryC (M.getFun f)
     undefined
{-
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
     typeArgs <- mapM C.zonk typeArgs'
     sizeArgs <- forM sizeArgs' \(t',s) ->
                   do t <- C.zonk t'
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
                       , ircFunType   = funTy
                       , ircArgTypes  = haveTs
                       , ircResType   = resT
                       , ircArgs      = ces
                       }


     res <- case compare haveArgs needArgs of
              EQ -> pure (IRCallFun call)
              LT -> pure (IRClosure call { ircResType = TFun needTs resT })
              GT -> C.unsupported
                      $ vcat [ "function over applied (higher order result)"
                             , "function:" <+> pp funName
                             , "type:" <+> pp funTy
                             , "applied to" <+> pp haveArgs <+> "arguments"
                             , "expected" <+> pp needArgs <+> "arguments"
                             , "expected params" <+> hsep (map pp argTs) <+> "arguments"
                             ]


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
            do yes <- C.caseBool x
               if yes then doITE more opt1 opt2 else doRes opt2
          GNotBool x ->
            do yes <- C.caseBool x
               if yes then doRes opt2 else doITE more opt1 opt2

          -- can consider dynamic check if all alternatives return
          -- the same type
          GNum x n ->
            do yes <- C.caseConst (toCryS (IRPolySize x)) EQ n
               if yes then doITE more opt1 opt2 else doRes opt2
          GNumFun tf as rel n ->
            do yes <- C.caseConst (toCryS (IRComputedSize tf as)) rel n
               if yes then doITE more opt1 opt2 else doRes opt2

  toCryS sz =
    case sz of
      IRFixedSize n         -> Cry.tNum n
      IRPolySize x          -> Cry.TVar (Cry.TVBound (irsName x))
      IRComputedSize tf as  -> Cry.TCon (Cry.TF tf) (map toCryS as)

  doRes res =
    case res of
      Found a  -> pure a
      NotFound -> bad ["NotFound"]
      ITE gs opt1 opt2 -> doITE gs opt1 opt2

  bad = panic "compileCall"


compileType :: Cry.Type -> C.ConvertM (Either Type StreamSize)
compileType ty =
  case Cry.kindOf ty of
    Cry.KNum  -> Right <$> compileStreamSizeType ty
    Cry.KType -> Left  <$> compileValType ty
    _         -> C.panic "compileTypes" ["Unexpecte higher order kind"]

-}


-- | Numeric on the left
matchParamInfo :: ParamInfo -> Cry.Type -> Maybe [Either Cry.Type Cry.Type]
matchParamInfo pi t =
  case pi of
    NumFixed n ->
      case Cry.tIsNat' t of
        Just n' -> guard (n == n') >> pure []
        _       -> Nothing
    NumVar _  -> guard (not (Cry.tIsInf t)) >> pure [Left t]
    TyBool    -> guard (Cry.tIsBit t) >> pure []
    TyNotBool -> guard (not (Cry.tIsBit t)) >> pure [Right t]
    TyAny     -> pure [Right t]

-- | Check if some concrete type arguments match a particular
-- function instance.  Returns (type arguments, size arguments)
matchFun ::
  (FunName, FunType) ->
  [Cry.Type] ->
  Maybe ([Cry.Type], [Cry.Type])
matchFun (f, funTy) tyArgs =
  do let FunInstance pi = irfnInstance f
     binds <- concat <$> zipWithM matchParamInfo pi tyArgs
     let (nums,vals) = partitionEithers binds
     let numPs = ftSizeParams funTy
         valPs = ftTypeParams funTy

     unless (length nums == length numPs) $
        panic "matchFun" ["Numeric argument mismatch"]
     unless (length vals == length valPs) $
        panic "matchFun" ["Value type argument mismatch"]

     pure (vals, nums)


selectInstance :: Cry.Name -> [Cry.Type] -> Type -> M.CryC ()
selectInstance f tyArgs tgtT =
  do instDB <- M.getFun f
     undefined


{-
matchType :: Type -> Type -> Maybe ()
matchType patTy argTy =
  case (patTy,argTy) of
    (TPoly x, 

    (TBool, TBool) -> undefined
    (TInteger, TInteger) -> undefined
  | TIntegerMod (IRSize tname)                    -- ^ Z
  | TRational                                     -- ^ Rational number
  | TFloat                                        -- ^ Floating point small
  | TDouble                                       -- ^ Floating point large
  | TWord (IRSize tname)                          -- ^ Bit vector

  | TArray (IRSize tname) (IRType tname)          -- ^ Array
  | TStream (IRStreamSize tname) (IRType tname)   -- ^ Iterator
  | TTuple [IRType tname]                         -- ^ Tuple

  | TFun [IRType tname] (IRType tname)            -- ^ Function types

-}




coerceTo :: Expr -> Type -> C.ConvertM Expr
coerceTo e tgtT =
  do let srcT = typeOf e

     case (srcT,tgtT) of

       (TStream (IRSize l1) t1, TArray l2 t2)
         | (l1,t1) == (l2,t2) -> pure (callPrim Collect [e] tgtT)

       (TStream (IRSize l1) TBool, TWord l2)
          | l1 == l2          -> pure (callPrim Collect [e] tgtT)

       (TArray l1 t1, TStream (IRSize l2) t2)
         | (l1,t1) == (l2,t2) -> pure (callPrim Iter [e] tgtT)

       (TWord l1, TStream (IRSize l2) TBool)
         | l1 == l2           -> pure (callPrim Iter [e] tgtT)

       -- somtimes the indexes are not exactly the same
       -- (e.g., `a * b` vs  `b * a`)
       (TWord _, TWord _) -> pure e

       -- Note that we are not checking the elements, but we probalby should
       (TArray _ _, TArray _ _) -> pure e

       -- Note that we are not checking the elements, but we probalby should
       (TStream _ _, TStream _ _) -> pure e

       _ | srcT == tgtT -> pure e
         | otherwise -> panic "adjustType"
                          [ "Cannot coerce types"
                          , "From: " ++ show (pp srcT)
                          , "To  : " ++ show (pp tgtT)
                          , "Expr: " ++ show (pp e)
                          ]


--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Simeple recursive equations, of the form:
-- xs = init # [ f a b c | a <- g1 | b <- g2 | c <- g3 ]
-- the generators may depend on the last `length init` elements of `xs`
-- or streams not depending on x

compileRecStream ::
  Name        {-^ Recursive stream name -} ->
  Cry.Expr    {-^ Definition -} ->
  C.ConvertM Expr
compileRecStream x def =
  do mb <- isAppend def
     case mb of
       Just (front,_back,ty,xs,ys)
         | isExtern xs ->
           do len  <- T.compileSizeType front
              maxHist <- case isKnownSize len of
                           Just n -> pure n
                           Nothing ->
                              C.unsupported "history needs to be constant"
              elTy <- T.compileValType ty
              hist <- compileExpr xs (TArray len elTy)
              (body,exts) <- compTail elTy maxHist ys
              pure $
                IRExpr $
                IRStream
                IRStreamExpr
                  { irsType     = irNameType x
                  , irsExterns  = exts
                  , irsInit     = hist
                  , irsNext     = body
                  }

       _ -> C.unsupported $ vcat [ "Recursive stream needs: xs # ys"
                                 , cryPP def
                                 ]

  where
  compTail elTy maxHist expr =
    case expr of
      Cry.ELocated _rng e1 -> compTail elTy maxHist e1
      Cry.EComp _len _ty res arms ->
        do rgens <- forM arms \case
                      [ms] -> compGen maxHist ms
                      _    -> C.unsupported "arm with multiple matches"
           let exts = [ ex     | (_,_,_,Just ex) <- rgens ]
               vars = [ (a,t) | (a,t,_,_) <- rgens ]
               mkLet (a,_,ex,_) e = IRExpr (IRLet a ex e)
           body <- C.withLocals vars (compileExpr res elTy)
           pure (foldr mkLet body rgens, exts)

      _ -> do e <- doRecGen elTy maxHist expr
              pure (e,[])


  compGen maxHist g =
    case g of
      Cry.Let {} -> C.unsupported "`let` in generator of recursive stream"
      Cry.From y len elTy expr ->
        do rty <- T.compileValType elTy
           (rexpr,histOrExt) <-
              if isExtern expr
                then
                  do extLen      <- T.compileStreamSizeType len
                     (extN,extE) <- doExternGen extLen rty expr
                     let val = callPrim Head [ IRExpr (IRVar extN) ] rty
                     pure (val, Just (extN,extE))
                else
                  do val <- doRecGen rty maxHist expr
                     pure (val, Nothing)

           let name = IRName { irNameName = NameId y
                             , irNameType = rty
                             }
           pure (name, elTy, rexpr, histOrExt)

  doExternGen len elTy e =
    do nameId <- C.doCryC M.newNameId
       let ty = TStream len elTy
           name = IRName { irNameName = nameId, irNameType = ty }
       ce <- compileExpr e ty
       pure (name, ce)


  doRecGen elTy n e =
    case e of
      Cry.ELocated _rng e1 -> doRecGen elTy n e1
      Cry.EVar x'
        | cryName == x' ->
          if 0 < n && n < maxSizeVal
             then let amt = (IRFixedSize (n-1), MemSize)
                  in pure (callSizePrim Hist [ amt ] elTy)
             else C.unsupported "non-positive or too large history"

      _ ->
        do mb <- isDrop e
           case mb of
             Nothing -> C.unsupported "only `drop` in rec. gen."
             Just (amt,e1) ->
               do iamt <- T.compileSizeType amt
                  case isKnownSize iamt of
                    Just i -> doRecGen elTy (n - i) e1
                    Nothing ->
                      C.unsupported "drop size argument needs to be static"


  cryName = case irNameName x of
              NameId y -> y
              _ -> panic "compileRecStream" ["Name is anon"]

  isExtern :: Cry.Expr -> Bool
  isExtern ce = not (cryName `Set.member` Cry.valDeps (Cry.freeVars ce))

  isAppend e =
    do mb <- isPrim "#" e
       let check ([front,back,elTy],[e1,e2]) = (front,back,elTy,e1,e2)
           check _ = panic "isAppend" ["Inavlid arguments"]
       pure (check <$> mb)

  isDrop e =
    do mb <- isPrim "drop" e
       let check ([front,_back,_elTy], [a]) = (front,a)
           check _ = panic "isDrop" ["Inavlid arguments"]
       pure (check <$> mb)

  isPrim :: Text -> Cry.Expr -> C.ConvertM (Maybe ([Cry.Type], [Cry.Expr]))
  isPrim name expr0 =
    case expr0 of
      Cry.ELocated _rng e1 -> isPrim name e1
      Cry.EApp {} ->
        do let (args',expr1)            = Cry.splitWhile Cry.splitApp expr0
               args                     = reverse args'
               (expr2,tyArgs,_profApps) = Cry.splitExprInst expr1
               expr                     = Cry.dropLocs expr2
           case expr of
             Cry.EVar fu ->
               do mbPrim <- C.doCryC (M.isPrimDecl fu)
                  pure
                    do p <- mbPrim
                       guard (p == Cry.prelPrim name)
                       pure (tyArgs,args)
             _ -> pure Nothing
      _ -> pure Nothing







-------------------------------------------------------------------------------

data CryRecEqn = CryRecEqn
  { ceqName :: Cry.Name
  , ceqTy   :: Cry.Type
  , ceqLen  :: Cry.Type
  , ceqElTy :: Cry.Type
  , ceqDef  :: Cry.Expr
  }

{- | Check if a recursive group is suitable for recursive stream compilation.
Currently we require that all definitions are monomorphic streams,
which should be the common case for locals (there can be still polymorphism
from the enclosing declaration). -}
isRecValueGroup :: [Cry.Decl] -> Either Doc [CryRecEqn]
isRecValueGroup = traverse isRecValDecl
  where
  isRecValDecl d =
    do def <- case Cry.dDefinition d of
                Cry.DExpr e -> pure e
                _           -> Left "Recursive equation with definition"
       ty <- fromMb "Recursive equation not monomorphic"
             $ Cry.isMono (Cry.dSignature d)
       (len,elTy) <- fromMb (vcat [ "Recursive equation is not a sequence"
                                  , cryPP d
                                  ])
                   $ Cry.tIsSeq ty
       pure CryRecEqn { ceqName = Cry.dName d
                      , ceqTy   = ty
                      , ceqLen  = len
                      , ceqElTy = elTy
                      , ceqDef  = def
                      }
  fromMb msg mb =
    case mb of
      Nothing -> Left msg
      Just a  -> Right a

compileRecursiveStreams :: [CryRecEqn] -> C.ConvertM Expr -> C.ConvertM Expr
compileRecursiveStreams defs k =
  case defs of
    [eqn] ->
      do (_,ty,x) <- getName eqn
         d <- compileRecStream x (ceqDef eqn)
         body <- C.withLocals [(x,ty)] k
         pure (IRExpr (IRLet x d body))
    _ -> C.unsupported "currently only single recursive equation"

  where
  getName eqn =
    do len  <- T.compileStreamSizeType (ceqLen eqn)
       elTy <- T.compileValType (ceqElTy eqn)
       let nm = ceqName eqn
       pure (nm, ceqTy eqn, IRName (NameId nm) (TStream len elTy))

