-- | Utilities for generating Rust code
-- Nothing here should be specific to the compiler.
module Cryptol.Compiler.Rust.Utils where

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Data.Position qualified as Rust
import Language.Rust.Syntax (QSelf(QSelf))

import Cryptol.Compiler.Error (panic)

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

-- | A block under construction
type PartialBlock a = ([RustStmt], a)

-- | Make a partial block with no statememtns.
justExpr :: a -> PartialBlock a
justExpr e = ([],e)

-- | Turn a partial block into an actual block.
finishBlock :: PartialBlock RustExpr -> RustBlock
finishBlock (xs,e) = block' xs e

dummySpan :: Rust.Span
dummySpan = Rust.Span Rust.NoPosition Rust.NoPosition

--------------------------------------------------------------------------------
-- Paths
simplePath :: Rust.Ident -> RustPath
simplePath n = Rust.Path False [Rust.PathSegment n Nothing ()] ()

simplePath' :: [Rust.Ident] -> RustPath
simplePath' ns = Rust.Path False [Rust.PathSegment n Nothing () | n <- ns] ()

pathWithTypes :: [Rust.Ident] -> [RustType] -> RustPath
pathWithTypes ns0 tys = Rust.Path False (go ns0) ()
  where
    go ns =
      case ns of
        [] -> panic "pathWithTypes" ["empty names"]
        [lst] -> [Rust.PathSegment lst (Just pathParams) ()]
        n:t -> Rust.PathSegment n Nothing () : go t
    pathParams = Rust.AngleBracketed [] tys [] ()

pathAddTypeSuffix :: RustPath -> [RustType] -> RustPath
pathAddTypeSuffix (Rust.Path global seg an) newTys = Rust.Path global seg' an
  where
  seg' = init seg ++ [modSegment (last seg)]

  modSegment (Rust.PathSegment name mbParams a) =
    Rust.PathSegment name (Just (modPathParams mbParams)) a

  modPathParams mbParams =
    case mbParams of
      Nothing -> Rust.AngleBracketed [] newTys [] ()
      Just params ->
        case params of
          Rust.Parenthesized {} -> panic "pathAddTypeSuffix" ["Parenthesized"]
          Rust.AngleBracketed _ _ (_:_) () ->
            panic "pathAddTypeSuffix" ["AngleBracketed with arg 3"]
          Rust.AngleBracketed lts curTys [] () ->
            Rust.AngleBracketed lts (curTys++newTys) [] ()


--------------------------------------------------------------------------------

typePath :: RustType -> RustPath -> RustExpr
typePath ty n = Rust.PathExpr [] (Just (QSelf ty 0)) n ()

pathExpr :: RustPath -> RustExpr
pathExpr p = Rust.PathExpr [] Nothing p ()

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

addrOf :: RustExpr -> RustExpr
addrOf e = Rust.AddrOf [] Rust.Immutable e ()

rustArray :: [RustExpr] -> RustExpr
rustArray es = Rust.Vec [] es ()

--------------------------------------------------------------------------------
-- Top Delcarations

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

-- | Make a simple use path
mkUse :: [Rust.Ident] -> RustItem
mkUse xs = Rust.Use [] Rust.InheritedV useTree ()
  where
  useTree = Rust.UseTreeSimple (simplePath' xs) Nothing ()

-- | Make a simple use path
mkUseGlob :: [Rust.Ident] -> RustItem
mkUseGlob xs = Rust.Use [] Rust.InheritedV useTree ()
  where
  useTree = Rust.UseTreeGlob (simplePath' xs) ()




--------------------------------------------------------------------------------
-- Literals and Constants

mkIntLit :: Rust.Suffix -> Integer -> RustLit
mkIntLit s i = Rust.Int Rust.Dec i s ()

mkU8Lit :: Integer -> RustLit
mkU8Lit = mkIntLit Rust.U8

mkU64Lit :: Integer -> RustLit
mkU64Lit = mkIntLit Rust.U64

mkUSizeLit :: Integer -> RustLit
mkUSizeLit = mkIntLit Rust.Us

litExpr :: RustLit -> RustExpr
litExpr l = Rust.Lit [] l ()



unitExpr :: RustExpr
unitExpr = tupleExpr []

tupleExpr :: [RustExpr] -> RustExpr
tupleExpr es = Rust.TupExpr [] es ()

arrayExpr :: [RustExpr] -> RustExpr
arrayExpr es = Rust.Vec [] es ()

--------------------------------------------------------------------------------
-- Types

inferType :: RustType
inferType = Rust.Infer ()

simpleType :: String -> RustType
simpleType i = Rust.PathTy Nothing path ()
  where
    path  = Rust.Path False [Rust.PathSegment ident Nothing ()] ()
    ident = Rust.mkIdent i

pathType :: RustPath -> RustType
pathType path = Rust.PathTy Nothing path ()

constType :: Integer -> RustType
constType n = Rust.ConstTy (litExpr (mkUSizeLit n)) ()

unitType :: RustType
unitType = tupleType []

tupleType :: [RustType] -> RustType
tupleType tys = Rust.TupTy tys ()

boolType :: RustType
boolType = simpleType "bool"

vectorOfType :: RustType -> RustType
vectorOfType elT = Rust.PathTy Nothing path ()
  where
  path   = Rust.Path False [ Rust.PathSegment "Vec" (Just params) () ] ()
  params = Rust.AngleBracketed [] [elT] [] ()

fixedArrayOfType :: RustType-> Integer -> RustType
fixedArrayOfType ty i = Rust.Array ty sizeExpr ()
  where
    sizeExpr = Rust.Lit [] (Rust.Int Rust.Dec i Rust.Unsuffixed ()) ()

refType :: RustType -> RustType
refType ty = Rust.Rptr Nothing Rust.Immutable ty ()
