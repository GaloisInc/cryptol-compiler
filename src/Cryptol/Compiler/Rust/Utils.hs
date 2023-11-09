-- | Utilities for generating Rust code
-- Nothing here should be specific to the compiler.
module Cryptol.Compiler.Rust.Utils where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.List(intersperse)
import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Data.Position qualified as Rust

import Cryptol.Compiler.Error (panic)

type RustSourceFile = Rust.SourceFile ()
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

typeQualifiedExpr :: RustType -> RustPath -> RustExpr
typeQualifiedExpr ty n = Rust.PathExpr [] (Just (Rust.QSelf ty 0)) n ()

pathExpr :: RustPath -> RustExpr
pathExpr p = Rust.PathExpr [] Nothing p ()

block :: [RustStmt] -> RustBlock
block stmts = Rust.Block stmts Rust.Normal ()

block' :: [RustStmt] -> RustExpr -> RustBlock
block' stmts expr = block (stmts ++ [stmtNoSemi expr])

stmtNoSemi :: RustExpr -> RustStmt
stmtNoSemi e = Rust.NoSemi e ()

assign :: RustExpr -> RustExpr -> RustStmt
assign lhs rhs = Rust.Semi (Rust.Assign [] lhs rhs ()) ()

mkRustCall :: RustExpr -> [RustExpr] -> RustExpr
mkRustCall fn args = Rust.Call [] fn args ()

callMethod :: RustExpr -> Rust.Ident -> [RustExpr] -> RustExpr
callMethod obj meth args =
  Rust.MethodCall [] obj meth Nothing args ()

callMacro :: RustPath -> [RustExpr] -> RustExpr
callMacro m es = Rust.MacExpr [] mac ()
  where
  mac       = Rust.Mac m args ()
  args      = Rust.Stream (intersperse comma (map exprToken es))
  exprToken = Rust.Tree . Rust.Token dummySpan . Rust.Interpolated
            . Rust.NtExpr . fmap (const dummySpan)
  comma     = Rust.Tree (Rust.Token dummySpan Rust.Comma)


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

rustIf :: RustExpr -> RustBlock -> RustBlock -> RustExpr
rustIf eTest eThen eElse =
  Rust.If [] eTest eThen (Just (Rust.BlockExpr [] eElse ())) ()

--------------------------------------------------------------------------------
-- Top Delcarations

sourceFile :: Maybe Rust.Name -> [RustItem] -> RustSourceFile
sourceFile nm = Rust.SourceFile nm []

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

mkFnItem_ ::
  Rust.Ident -> RustGenerics -> [(Rust.Ident, RustType)] ->
  RustBlock ->  RustItem
mkFnItem_ name generics params body =
  Rust.Fn [] vis name decl unsafety constness abi generics body ()
  where
    vis = Rust.PublicV
    unsafety = Rust.Normal
    constness = Rust.NotConst
    abi = Rust.Rust
    mkArg (n, t) = Rust.Arg (Just $ identPat n) t ()
    decl =
      Rust.FnDecl (mkArg <$> params) Nothing False ()



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

-- | Declare a public module.
pubMod :: Rust.Ident -> RustItem
pubMod i = Rust.Mod [] Rust.PublicV i Nothing ()



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

boolLit :: Bool -> RustLit
boolLit b = Rust.Bool b Rust.Unsuffixed ()

strLit :: Text -> RustLit
strLit s = Rust.Str (Text.unpack s) Rust.Cooked Rust.Unsuffixed ()

litExpr :: RustLit -> RustExpr
litExpr l = Rust.Lit [] l ()



unitExpr :: RustExpr
unitExpr = tupleExpr []

tupleExpr :: [RustExpr] -> RustExpr
tupleExpr es = Rust.TupExpr [] es ()

tupleSelect :: RustExpr -> Int -> RustExpr
tupleSelect e i = Rust.TupField [] e i ()

arrayExpr :: [RustExpr] -> RustExpr
arrayExpr es = Rust.Vec [] es ()

indexExpr :: RustExpr-> RustExpr -> RustExpr
indexExpr a i = Rust.Index [] a i ()


--------------------------------------------------------------------------------
-- Types

inferType :: RustType
inferType = Rust.Infer ()

simpleType :: String -> RustType
simpleType i = Rust.PathTy Nothing path ()
  where
    path  = Rust.Path False [Rust.PathSegment ident Nothing ()] ()
    ident = Rust.mkIdent i


typeQualifiedType:: RustType -> RustPath -> RustType
typeQualifiedType ty n = Rust.PathTy (Just (Rust.QSelf ty 0)) n ()

pathType :: RustPath -> RustType
pathType path = Rust.PathTy Nothing path ()

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

fixedArrayOfType :: RustType -> Integer -> RustType
fixedArrayOfType ty i = Rust.Array ty sizeExpr ()
  where
    sizeExpr = Rust.Lit [] (Rust.Int Rust.Dec i Rust.Unsuffixed ()) ()

sliceType :: RustType -> RustType
sliceType ty = Rust.Slice ty ()

refType :: RustType -> RustType
refType ty = Rust.Rptr Nothing Rust.Immutable ty ()

mutRefType :: RustType -> RustType
mutRefType ty = Rust.Rptr Nothing Rust.Mutable ty ()
