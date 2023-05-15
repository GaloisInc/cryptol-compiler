module Cryptol.Compiler.Rust.CodeGen where

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
-- import Cryptol.Compiler.Rust.Types


nameFromIRName :: IRName t n -> n
nameFromIRName (IRName n _) = n

mkRustCall :: RustExpr -> [RustExpr] -> RustExpr
mkRustCall fn args = Rust.Call [] fn args ()

mkClosure :: [Rust.Ident] -> [RustStmt] -> RustExpr -> RustExpr
mkClosure args stmts expr = Rust.Closure [] move captureBy fnDecl fnExpr ()
  where
    move = Rust.Movable
    captureBy = Rust.Value
    mkArg a = Rust.Arg (Just $ identPat a) (Rust.Infer ()) ()
    args' = mkArg <$> args
    variadic = False
    fnDecl = Rust.FnDecl args' Nothing variadic ()
    fnExpr = blockExprIfNeeded stmts expr


blockExprIfNeeded :: [RustStmt] -> RustExpr -> RustExpr
blockExprIfNeeded stmts e =
  case stmts of
    [] -> e
    _ -> Rust.BlockExpr [] (block (stmts ++ [Rust.NoSemi e ()])) ()

identPat :: Rust.Ident -> Rust.Pat ()
identPat ident = Rust.IdentP bindingMode ident Nothing ()
  where
    bindingMode = Rust.ByValue Rust.Immutable


callPrim :: IRPrim -> [RustExpr] -> Gen RustExpr
callPrim = undefined


doGenExpr :: Expr -> Gen RustExpr
doGenExpr e =
  do (stmts,expr) <- genExpr e
     pure (blockExprIfNeeded stmts expr)

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
      do  let args' = nameFromIRName <$> args
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


