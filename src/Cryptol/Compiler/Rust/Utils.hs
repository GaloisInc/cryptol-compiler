-- | Utilities for generating Rust code
-- Nothing here should be specific to the compiler.
module Cryptol.Compiler.Rust.Utils where

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Data.Position qualified as Rust
import Language.Rust.Syntax (QSelf(QSelf))

type RustPath     = Rust.Path ()
type RustType     = Rust.Ty ()
type RustExpr     = Rust.Expr ()
type RustStmt     = Rust.Stmt ()
type RustBlock    = Rust.Block ()
type RustItem     = Rust.Item ()
type RustGenerics = Rust.Generics ()
type RustLifetimeDef = Rust.LifetimeDef ()
type RustTyParam = Rust.TyParam ()
type RustWherePredicate = Rust.WherePredicate ()
type RustWhereClause = Rust.WhereClause ()
type RustLit = Rust.Lit ()

dummySpan :: Rust.Span
dummySpan = Rust.Span Rust.NoPosition Rust.NoPosition

simplePath :: Rust.Ident -> RustPath
simplePath n = Rust.Path False [Rust.PathSegment n Nothing ()] ()

simplePath' :: [Rust.Ident] -> RustPath
simplePath' ns = Rust.Path False [Rust.PathSegment n Nothing () | n <- ns] ()

typePath :: RustType -> RustPath -> RustExpr
typePath ty n = Rust.PathExpr [] (Just (QSelf ty 0)) n ()

pathExpr :: RustPath -> RustExpr
pathExpr p = Rust.PathExpr [] Nothing p ()

pathType :: RustPath -> RustType
pathType path = Rust.PathTy Nothing path ()

block :: [RustStmt] -> RustBlock
block stmts = Rust.Block stmts Rust.Normal ()

block' :: [RustStmt] -> RustExpr -> RustBlock
block' stmts expr = block (stmts ++ [stmtNoSemi expr])

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

mkGenerics ::
  [RustLifetimeDef] ->
  [RustTyParam] ->
  RustWhereClause ->
  RustGenerics
mkGenerics lts tps wc = Rust.Generics lts tps wc ()

blockExprIfNeeded :: [RustStmt] -> RustExpr -> RustExpr
blockExprIfNeeded stmts e =
  case stmts of
    [] -> e
    _ -> Rust.BlockExpr [] (block (stmts ++ [Rust.NoSemi e ()])) ()

identPat :: Rust.Ident -> Rust.Pat ()
identPat ident = Rust.IdentP bindingMode ident Nothing ()
  where
    bindingMode = Rust.ByValue Rust.Immutable

todoExp :: String -> RustExpr
todoExp txt = Rust.MacExpr [] todoMac ()
  where
  todoMac = Rust.Mac (simplePath "todo") (Rust.Stream [Rust.Tree arg]) ()
  arg = Rust.Token dummySpan (Rust.LiteralTok str Nothing)
  str = Rust.StrTok txt

todoBlock :: String -> RustBlock
todoBlock txt = block [stmtNoSemi (todoExp txt)]

exprStmt :: RustExpr -> RustStmt
exprStmt e = Rust.NoSemi e ()

mkFnItem ::
  Rust.Ident -> RustGenerics -> [(Rust.Ident, RustType)] -> RustType ->
  RustBlock ->  RustItem
mkFnItem name generics params returnTy body =
  Rust.Fn [] vis name decl unsafety constness abi generics body ()
  where
    vis = Rust.PublicV
    unsafety = Rust.Normal
    constness = Rust.NotConst
    abi = Rust.Rust
    mkArg (n, t) = Rust.Arg (Just $ identPat n) t ()
    decl =
      Rust.FnDecl (mkArg <$> params) (Just returnTy) False ()

mkIntLit :: Rust.Suffix -> Integer -> RustLit
mkIntLit s i = Rust.Int Rust.Dec i s ()

mkU64Lit :: Integer -> RustLit
mkU64Lit = mkIntLit Rust.U64

mkUSizeLit :: Integer -> RustLit
mkUSizeLit = mkIntLit Rust.Us

litExpr :: RustLit -> RustExpr
litExpr l = Rust.Lit [] l ()

unitExpr :: RustExpr
unitExpr = Rust.TupExpr [] [] ()


--------------------------------------------------------------------------------

rustSimpleType :: String -> RustType
rustSimpleType i = Rust.PathTy Nothing path ()
  where
    path  = Rust.Path False [Rust.PathSegment ident Nothing ()] ()
    ident = Rust.mkIdent i

unitType :: RustType
unitType = tupleType []

tupleType :: [RustType] -> RustType
tupleType tys = Rust.TupTy tys ()

boolType :: RustType
boolType = rustSimpleType "bool"

vectorOfType :: RustType -> RustType
vectorOfType elT = Rust.PathTy Nothing path ()
  where
  path   = Rust.Path False [ Rust.PathSegment "Vec" (Just params) () ] ()
  params = Rust.AngleBracketed [] [elT] [] ()

fixedArrayOfType :: RustType-> Integer -> RustType
fixedArrayOfType ty i = Rust.Array ty sizeExpr ()
  where
    sizeExpr = Rust.Lit [] (Rust.Int Rust.Dec i Rust.Unsuffixed ()) ()

fixedSizeWordType :: Integer -> RustType
fixedSizeWordType bits = Rust.MacTy mac ()
  where
      mac = Rust.Mac (simplePath "BitVec") tokenStream ()
      -- tokenStream = Rust.Tree tt
      tokenStream = Rust.Tree lengthTok
      tt = Rust.Delimited dummySpan Rust.Paren (Rust.Tree lengthTok)
      lengthTok = Rust.Token dummySpan $ Rust.LiteralTok (Rust.IntegerTok (show bits)) Nothing

