module Cryptol.Compiler.Simple where

import qualified Cryptol.Utils.Ident as Cry
import qualified Cryptol.TypeCheck.AST as Cry

import Cryptol.Compiler.IR
import Cryptol.Compiler.Monad


compileModule :: Cry.Module -> CryC [Decl]
compileModule m =
  do dss <- mapM compileDeclGroup (Cry.mDecls m)
     pure (concat dss)

compileDeclGroup :: Cry.DeclGroup -> CryC [Decl]
compileDeclGroup dg =
  case dg of
    Cry.NonRecursive d -> compileDecl d
    Cry.Recursive ds   -> compileRecDecls ds

compileDecl :: Cry.Decl -> CryC [Decl]
compileDecl d =
  case Cry.dDefinition d of
    Cry.DPrim       -> pure []
    Cry.DForeign {} -> pure [] -- XXX: Revisit
    Cry.DExpr e     -> undefined

compileRecDecls :: [Cry.Decl] -> CryC [Decl]
compileRecDecls d = undefined


prepExprDecl ::
  Cry.Expr -> ([Cry.TParam], [Cry.Prop], [(Cry.Name,Cry.Type)],Cry.Expr)
prepExprDecl expr = (tparams, quals, args, body)
  where
  (tparams,expr1) = Cry.splitWhile Cry.splitTAbs     expr
  (quals,expr2)   = Cry.splitWhile Cry.splitProofAbs expr1
  (args,body)     = Cry.splitWhile Cry.splitAbs      expr2



compileExpr :: Cry.Expr -> CryC Expr
compileExpr expr0 =

  do let (expr1,tyArgs,_profApps) = Cry.splitExprInst expr0
         (args,expr2)             = Cry.splitWhile Cry.splitApp expr1
         expr                     = Cry.dropLocs expr2

     cargs <- mapM compileExpr args

     case expr of
       Cry.EVar x -> compileVar x tyArgs cargs

       Cry.EList es t ->
         do ces <- mapM compileExpr es
            let t = undefined
            pure (IRExpr (IRPrim (Array t ces)))

       Cry.ETuple es ->
         do ces <- mapM compileExpr es
            pure (IRExpr (IRPrim (Tuple ces)))

       Cry.ERec rm               -> undefined
       Cry.ESel e sel            -> undefined
       Cry.ESet ty recE sel valE -> undefined

       Cry.EIf eCond eThen eElse ->
         do ceCond <- compileExpr eCond
            ceThen <- compileExpr eThen
            ceElse <- compileExpr eElse
            pure (IRExpr (IRIf ceCond ceThen ceElse))

       Cry.EComp tyLen tyEl expr ms -> undefined

       Cry.EApp efun earg        -> unexpected "EApp"
       Cry.EAbs x t e            -> undefined

       Cry.ELocated rng e        -> unexpected "ELocated"
       Cry.ETAbs {}              -> unexpected "ETAbs"
       Cry.ETApp {}              -> unexpected "ETapp"
       Cry.EProofAbs {}          -> unexpected "EProofAbs"
       Cry.EProofApp expr        -> unexpected "EProofApp"

       Cry.EWhere expr ds        -> undefined
       Cry.EPropGuards guars ty  -> undefined

  where
  unexpected msg = panic "compileExpr" [msg]


compileVar ::
  Cry.Name -> [Cry.Type] -> [Expr] -> CryC Expr
compileVar x ts args =
  do mbPrim <- isPrimDecl x
     case mbPrim of
       Just p -> compilePrim p ts args
       Nothing -> undefined

compilePrim ::
  Cry.PrimIdent -> [Cry.Type] -> [Expr] -> CryC Expr
compilePrim p ts args = undefined






















