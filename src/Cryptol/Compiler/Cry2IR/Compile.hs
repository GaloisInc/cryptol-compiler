module Cryptol.Compiler.Cry2IR.Compile where

import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP(pp,cryPP,(<+>))
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.Cry2IR.Monad qualified as S
import Cryptol.Compiler.Cry2IR.Specialize
import Cryptol.Compiler.Cry2IR.InstanceMap


compileModule :: Cry.Module -> M.CryC ()
compileModule m = mapM_ compileDeclGroup (Cry.mDecls m)

compileDeclGroup :: Cry.DeclGroup -> M.CryC ()
compileDeclGroup dg =
  case dg of
    Cry.NonRecursive d -> compileTopDecl (toPrim d)
    Cry.Recursive ds   -> mapM_ (compileTopDecl . toPrim) ds
  where
  toPrim d = d { Cry.dDefinition = Cry.DPrim }

compileTopDecl :: Cry.Decl -> M.CryC ()
compileTopDecl d =
  debugWrap

  do -- M.doIO (print (pp cname <+> ":" <+> cryPP (Cry.dSignature d)))
     insts <-
       case Cry.dDefinition d of
         Cry.DPrim       ->
           do insts <- compilePrimDecl (Cry.dSignature d)
              pure [ (i,ty,IRFunPrim (ftParams ty)) | (i,ty) <- insts ]
         Cry.DForeign {} -> M.unsupported "Foregin declaration" -- XXX: Revisit
         Cry.DExpr e ->
           do let (as,ps,xs,body) = prepExprDecl e
              M.withCryLocals xs
                do resTcry <- M.getTypeOf body
                   compileFunDecl as ps (map snd xs) resTcry \args resT ->
                      do let nms = zipWith IRName (map fst xs) args
                         def <- S.doCryCWith (M.withIRLocals nms) (compileExpr body)
                         pure (IRFunDef nms def)

     let decls = map mkDecl insts
         mbMap = instanceMapFromList
                   [ (irfnInstance (irfName fd), fd) | fd <- decls ]
     case mbMap of
       Right a  -> M.addCompiled cname a
       Left err -> M.unsupported err

  where
  cname = Cry.dName d

  mkDecl (i,ty,def) =
    IRFunDecl
      { irfName =
          IRFunName
            { irfnName     = cname
            , irfnInstance = i
            , irfnResult   = ftResult ty
            }

      , irfTParams    = ftTypeParams ty
      , irfTraits     = ftTraits ty
      , irfSizeParams = ftSizeParams ty
      , irfDef        = def
      }

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


compileExpr :: Cry.Expr -> S.SpecM Expr
compileExpr expr0 =

  do let (args,expr1)             = Cry.splitWhile Cry.splitApp expr0
         (expr2,tyArgs,_profApps) = Cry.splitExprInst expr1
         expr                     = Cry.dropLocs expr2

     cargs <- mapM compileExpr args

     case expr of

       Cry.EVar x -> compileVar x tyArgs cargs

       -- NOTE: if we are making a word, especially a small one
       Cry.EList es t ->
         do it  <- compileValType t
            ces <- mapM compileExpr es
            S.unsupported "list" -- pure (IRExpr (IRPrim (Array it ces)))

       Cry.ETuple es ->
         do ces <- mapM compileExpr es
            S.unsupported "tuple" -- pure (IRExpr (IRPrim (Tuple ces)))

       -- XXX
       Cry.ERec rm               -> S.unsupported "ERec"
       Cry.ESel e sel            -> S.unsupported "ESel"
       Cry.ESet ty recE sel valE -> S.unsupported "ESet"

       Cry.EIf eCond eThen eElse ->
         do ceCond <- compileExpr eCond
            ceThen <- compileExpr eThen
            ceElse <- compileExpr eElse
            pure (IRExpr (IRIf ceCond ceThen ceElse))

       Cry.EComp tyLen tyEl expr ms -> S.unsupported "EComp"

       Cry.EApp efun earg        -> unexpected "EApp"
       Cry.EAbs {}               -> S.unsupported "EAbs"

       Cry.ELocated rng e        -> unexpected "ELocated"
       Cry.ETAbs {}              -> unexpected "ETAbs"
       Cry.ETApp {}              -> unexpected "ETApp"
       Cry.EProofAbs {}          -> unexpected "EProofAbs"
       Cry.EProofApp {}          -> unexpected "EProofApp"

       Cry.EWhere expr ds        -> S.unsupported "EWhere"
       Cry.EPropGuards guars ty  -> S.unsupported "EPropGuards"

  where
  unexpected msg = panic "compileExpr" [msg]


compileVar :: Cry.Name -> [Cry.Type] -> [Expr] -> S.SpecM Expr
compileVar x ts args =
  do mb <- S.doCryC (M.getLocal x)
     case mb of
       Nothing -> compileCall x ts args
       Just n ->
         case (ts,args) of
           (_ : _, _) -> S.unsupported "Polymorphic locals"
           (_, _ : _) -> S.unsupported "Funciton local"
           ([],[])    -> pure (IRExpr (IRVar n))


-- XXX: Should lazy primitives (e.g., `\/') be handled specially here,
-- or in a later pass?
compileCall :: Cry.Name -> [Cry.Type] -> [Expr] -> S.SpecM Expr
compileCall f ts es =
  do instDB <- S.doCryC (M.getFun f)
     tys    <- mapM compileType ts
     (funNameUninst,funTy) <-
        case lookupInstance tys instDB of
          ITE gs opt1 opt2 -> doITE gs opt1 opt2
          Found a -> pure a
          NotFound ->
            bad
              [ "Missing instance"
              , "Function: " ++ show (pp f)
              , "Instance: " ++ show [ either pp pp x | x <- tys ]
              ]
     let (typeArgs,sizeArgs) = makeTArgs [] [] (irfnInstance funNameUninst) tys
         su = foldr (uncurry suAddType) suEmpty
            $ zip (ftTypeParams funTy) typeArgs
         funName = funNameUninst { irfnResult =
                                     apSubst su (irfnResult funNameUninst) }

     pure (IRExpr (IRCall funName typeArgs sizeArgs es))
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












