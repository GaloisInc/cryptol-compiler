module Cryptol.Compiler.Simple where

import qualified Cryptol.Utils.Ident as Cry
import qualified Cryptol.TypeCheck.AST as Cry

import Cryptol.Compiler.IR
import Cryptol.Compiler.Monad


compileModule :: Cry.Module -> CryC [IRDecl Cry.Name]
compileModule m =
  do dss <- mapM compileDeclGroup (Cry.mDecls m)
     pure (concat dss)

compileDeclGroup :: Cry.DeclGroup -> CryC [IRDecl Cry.Name]
compileDeclGroup dg =
  case dg of
    Cry.NonRecursive d -> compileDecl d
    Cry.Recursive ds   -> compileRecDecls ds

compileDecl :: Cry.Decl -> CryC [IRDecl Cry.Name]
compileDecl d =
  case Cry.dDefinition d of
    Cry.DPrim       -> pure []
    Cry.DForeign {} -> pure [] -- XXX: Revisit
    Cry.DExpr e     -> undefined

compileRecDecls :: [Cry.Decl] -> CryC [IRDecl Cry.Name]
compileRecDecls d = undefined


prepExprDecl ::
  Cry.Expr -> ([Cry.TParam], [Cry.Prop], [(Cry.Name,Cry.Type)],Cry.Expr)
prepExprDecl expr = (tparams, quals, args, body)
  where
  (tparams,expr1) = Cry.splitWhile Cry.splitTAbs     expr
  (quals,expr2)   = Cry.splitWhile Cry.splitProofAbs expr1
  (args,body)     = Cry.splitWhile Cry.splitAbs      expr2



compileExpr :: Cry.Expr -> IRType -> CryC (IRExpr Cry.Name)
compileExpr expr0 tgtTy =
  let (expr1,tyArgs,_profApps) = Cry.splitExprInst expr0
      (args,expr2)             = Cry.splitWhile Cry.splitApp expr1
      expr                     = Cry.dropLocs expr2 -- XXX: maybe do something?
  in
  case expr of
    Cry.EVar x                -> compileVar x tyArgs args tgtTy

    Cry.EList es t            -> undefined
    Cry.ETuple es             -> undefined
    Cry.ERec rm               -> undefined
    Cry.ESel e sel            -> undefined
    Cry.ESet ty recE sel valE -> undefined
    Cry.EIf eCond eThen eElse -> undefined
    Cry.EComp tyLen tyEl expr ms -> undefined


    Cry.EApp efun earg        -> undefined
    Cry.EAbs x t e            -> undefined

    Cry.ELocated rng e        -> panic "compileExpr" ["ELocated"]
    Cry.ETAbs {}              -> panic "compileExpr" ["ETAbs"]
    Cry.ETApp {}              -> panic "compileExpr" ["ETapp"]
    Cry.EProofAbs {}          -> panic "compileExpr" ["EProofAbs"]
    Cry.EProofApp expr        -> panic "compileExpr" ["EProofApp"]

    Cry.EWhere expr ds        -> undefined
    Cry.EPropGuards guars ty  -> undefined


compileVar ::
  Cry.Name -> [Cry.Type] -> [Cry.Expr] -> IRType -> CryC (IRExpr Cry.Name)
compileVar x ts args tgtTy =
  do mbPrim <- isPrimDecl x
     case mbPrim of
       Just p -> compilePrim p ts args tgtTy
       Nothing -> undefined

compilePrim ::
  Cry.PrimIdent -> [Cry.Type] -> [Cry.Expr] -> IRType -> CryC (IRExpr Cry.Name)
compilePrim p ts args tgtTy = undefined






















