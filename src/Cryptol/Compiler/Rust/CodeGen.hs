module Cryptol.Compiler.Rust.CodeGen where

import Language.Rust.Syntax qualified as Rust

import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
-- import Cryptol.Compiler.Rust.Types


callPrim :: IRPrim -> [RustExpr] -> Gen RustExpr
callPrim = undefined

doGenExpr :: Expr -> Gen RustExpr
doGenExpr e =
  do (stmts,expr) <- genExpr e
     pure (blockExprIfNeeded stmts expr)

-- | Generate
genExpr :: Expr -> Gen ([RustStmt], RustExpr)
genExpr (IRExpr e0) =
  let justExpr e = ([],e)
  in
  case e0 of
    IRVar (IRName name _) -> justExpr <$> lookupNameId name

    IRCallFun call ->
      justExpr <$>
      do  args' <- doGenExpr `traverse` ircArgs call
          case ircFun call of
            IRFunVal fnIR ->
              do  fnExpr <- doGenExpr fnIR
                  pure $ mkRustCall fnExpr args'

            IRTopFun tf ->
              do  name <- lookupFunName (irtfName tf)
                  case name of
                    Left prim -> callPrim prim args'
                    Right nameExpr -> pure $ mkRustCall nameExpr args'

    IRClosure call -> undefined

    IRLam args expr ->
      justExpr <$>
      do  let args' = irNameName <$> args
          args'' <- bindLocal addLocalVar `traverse` args'
          (lamStmt, lamE) <- genExpr expr
          pure $ mkClosure args'' lamStmt lamE


    IRIf eTest eThen eElse ->
      justExpr <$>
      do  eTest' <- doGenExpr eTest
          (eThenStmts, eThen') <- genExpr eThen
          elseBlockExpr <- doGenExpr eElse
          let thenBlock = block (eThenStmts ++ [exprStmt eThen'])
              eif = Rust.If [] eTest' thenBlock (Just elseBlockExpr) ()

          pure eif

    IRLet (IRName name _) eBound eIn ->
      do  eBound'    <- doGenExpr eBound
          boundIdent <- bindLocal addLocalVar name
          let -- TODO add explicit type?
              ty = Nothing
              letBind = Rust.Local (identPat boundIdent) ty (Just eBound') [] ()
          (stms,e) <- genExpr eIn
          pure (letBind:stms,e)


