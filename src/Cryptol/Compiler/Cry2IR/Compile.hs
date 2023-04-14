module Cryptol.Compiler.Cry2IR.Compile where

import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.IR
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.Cry2IR.Monad qualified as S
import Cryptol.Compiler.Cry2IR.Specialize

{-
compileModule :: Cry.Module -> CryC [Decl]
compileModule m

   -- XXX: Skip the Prelude for now, as it is too difficult :-)
  | Cry.mName m == Cry.preludeName = pure []

  | otherwise =
  do dss <- mapM compileDeclGroup (Cry.mDecls m)
     pure (concat dss)

compileDeclGroup :: Cry.DeclGroup -> CryC [Decl]
compileDeclGroup dg =
  case dg of
    Cry.NonRecursive d -> compileDecl d
    Cry.Recursive ds   -> compileRecDecls ds
-}

{-
compileTopDecl :: Cry.Decl -> M.CryC IRFunDecl
compileTopDecl d =
  case Cry.dDefinition d of
    Cry.DPrim -> pure []
    Cry.DForeign {} -> unsupported "Foregin declaration" -- XXX: Revisit
    Cry.DExpr e ->
      do let (as,ps,xs,body) = prepExprDecl e
         unless (null as && null ps)
                (unsupported "XXX: polymorphic declarations")

         let nm = Cry.dName d
         ps  <- mapM compileParam xs
         def <- withLocals ps (compileExpr body)
         let t = typeOf def
         pure [ IRFun (IRName nm t) [ x | (_,_,x) <- ps ] def ]
-}

{-
compileRecDecls :: [Cry.Decl] -> CryC [Decl]
compileRecDecls _ = pure [] -- XXX

compileParam :: (Cry.Name, Cry.Type) -> CryC (Cry.Name, Cry.Schema, Name)
compileParam (x,t) =
  do irt <- compileValType t
     let s = Cry.tMono t
     pure (x, s, IRName x irt)
-}

-- | Identify expressions that are functions
prepExprDecl ::
  Cry.Expr -> ([Cry.TParam], [Cry.Prop], [(Cry.Name,Cry.Type)],Cry.Expr)
prepExprDecl expr = (tparams, quals, args, body)
  where
  (tparams,expr1) = Cry.splitWhile Cry.splitTAbs     expr
  (quals,expr2)   = Cry.splitWhile Cry.splitProofAbs expr1
  (args,body)     = Cry.splitWhile Cry.splitAbs      expr2


{-

compileExpr :: Cry.Expr -> CryC Expr
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
            pure (IRExpr (IRPrim (Array it ces)))

       Cry.ETuple es ->
         do ces <- mapM compileExpr es
            pure (IRExpr (IRPrim (Tuple ces)))

       -- XXX
       Cry.ERec rm               -> unsupported "ERec"
       Cry.ESel e sel            -> unsupported "ESel"
       Cry.ESet ty recE sel valE -> unsupported "ESet"

       Cry.EIf eCond eThen eElse ->
         do ceCond <- compileExpr eCond
            ceThen <- compileExpr eThen
            ceElse <- compileExpr eElse
            pure (IRExpr (IRIf ceCond ceThen ceElse))

       Cry.EComp tyLen tyEl expr ms -> unsupported "EComp"

       Cry.EApp efun earg        -> unexpected "EApp"
       Cry.EAbs {}               -> unsupported "EAbs"

       Cry.ELocated rng e        -> unexpected "ELocated"
       Cry.ETAbs {}              -> unexpected "ETAbs"
       Cry.ETApp {}              -> unexpected "ETApp"
       Cry.EProofAbs {}          -> unexpected "EProofAbs"
       Cry.EProofApp {}          -> unexpected "EProofApp"

       Cry.EWhere expr ds        -> unsupported "EWhere"
       Cry.EPropGuards guars ty  -> unsupported "EPropGuards"

  where
  unexpected msg = panic "compileExpr" [msg]
-}


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


compileCall :: Cry.Name -> [Cry.Type] -> [Expr] -> S.SpecM Expr
compileCall f ts es =
  do tys <- mapM compileType ts
     undefined


compileType :: Cry.Type -> S.SpecM (Either Type StreamSize)
compileType ty =
  case Cry.kindOf ty of
    Cry.KNum  -> Right <$> compileStreamSizeType ty
    Cry.KType -> Left  <$> compileValType ty
    _         -> S.panic "compileTypes" ["Unexpecte higher order kind"]












