module Cryptol.Compiler.Rust.Utils where

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Data.Position qualified as Rust

type RustPath   = Rust.Path ()
type RustType   = Rust.Ty ()
type RustExpr   = Rust.Expr ()
type RustStmt   = Rust.Stmt ()
type RustBlock  = Rust.Block ()
type RustItem   = Rust.Item ()

dummySpan :: Rust.Span
dummySpan = Rust.Span Rust.NoPosition Rust.NoPosition

simplePath :: Rust.Ident -> Rust.Path ()
simplePath n = Rust.Path True [Rust.PathSegment n Nothing ()] ()

block :: [RustStmt] -> RustBlock
block stmts = Rust.Block stmts Rust.Normal ()

stmtNoSemi :: RustExpr -> RustStmt
stmtNoSemi e = Rust.NoSemi e ()

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

todoExp :: RustExpr
todoExp = Rust.MacExpr [] todoMac ()
  where
  todoMac = Rust.Mac (simplePath "todo") (Rust.Stream []) ()

todoBlock :: RustBlock
todoBlock = block [stmtNoSemi todoExp]

exprStmt :: RustExpr -> RustStmt
exprStmt e = Rust.NoSemi e ()




