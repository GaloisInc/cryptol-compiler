module Cryptol.Compiler.Rust.CompilePrim where

import Data.Text(Text)
import Data.Text qualified as Text
import qualified Language.Rust.Syntax as Rust
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.Error (panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils

data PrimArgs = PrimArgs
  { primTypesOfArgs   :: [Type]         -- ^ Types of arguments (primArgs)
  , primTypeOfResult  :: Type           -- ^ Type of result
  , primTypeArgs      :: [RustType]     -- ^ Compiled type arguemt
  , primLenArgs       :: [RustExpr]     -- ^ Compile length arguments
  , primSizeArgs      :: [RustExpr]     -- ^ Compiled size arguments
  , primArgs          :: [RustExpr]     -- ^ Compiled normal arguments
  }

instance PP PrimArgs where
  pp args =
    vcat
      [ "--- PrimArgs ---------------------------------"
      , parens (commaSep (map pp (primTypesOfArgs args))) <+> "->" <+>
                              pp (primTypeOfResult args)
      , " "
      , "types:"      <+> ppField (primTypeArgs args)
      , "lengths:"    <+> ppField (primLenArgs args)
      , "sizes:"      <+> ppField (primSizeArgs args)
      , "arguments:"  <+> ppField (primArgs args)
      , "---------------------------------------------------"
      ]
    where
    ppField :: RustPP a => [a] -> Doc
    ppField = commaSep . map rustPP

unsupportedPrim :: Doc -> PrimArgs -> Rust a
unsupportedPrim nm args =
  unsupported (vcat [ "primitive" <+> nm, pp args ])


-- | Is this a constructor primitive.
-- If so, it takes ownership of its arguments.
primIsConstructor :: IRPrim -> Rust Bool
primIsConstructor prim =
  case prim of
    CryPrim {}  -> pure False

    ArrayLit    -> pure True
    ArrayLookup -> pure False

    Tuple       -> pure True
    TupleSel {} -> pure False

    EqSize      -> pure False
    LtSize      -> pure False
    LeqSize     -> pure False

    -- XXX: are these OK?
    Map         -> pure True
    FlatMap     -> pure True 
    Zip         -> pure True 

    ArrayToStream -> pure True
    ArrayToWord   -> pure True
    WordToStream  -> pure True
    StreamToWord  -> pure True
    StreamToArray -> pure True

    Head          -> pure False
    Hist          -> pure False

  where
  notYet = unsupported ("primitive" <+> pp prim)

-- | Emit code for a primitve.
compilePrim :: IRPrim -> PrimArgs -> Rust RustExpr
compilePrim name args =
  case name of
    CryPrim p   -> compileCryptolPrim p args

    ArrayLit    -> pure (callMacro (simplePath "vec") (primArgs args))

    ArrayLookup ->
      case (primArgs args, primSizeArgs args) of
        ([a],[i]) -> pure (indexExpr a i)
        _ -> bad

    WordLookup ->
      size1 \i ->
      arg1  \a ->
        pure (indexExpr a i)

    StreamToArray ->
      arg1 \s -> pure (callMethod s "collect" [])
                -- XXX: type? specify that we want owned?

    Tuple -> pure (tupleExpr (primArgs args))
    TupleSel n _all ->
      arg1 \x -> pure (tupleSelect x n)

    _ -> pure (todoExp (show (pp name))) -- unsupportedPrim (pp name) args
  where
  bad = panic "compilePrim"
          [ "Malformed primitive arguments:"
          , show (pp args)
          ]
  arg1 f =
    case primArgs args of
      [a] -> f a
      _ -> bad

  size1 f =
    case primSizeArgs args of
      [a] -> f a
      _ -> bad




compileCryptolPrim :: Cry.PrimIdent -> PrimArgs -> Rust RustExpr
compileCryptolPrim p@(Cry.PrimIdent mo name) args
  | mo == Cry.preludeName = compileCryptolPreludePrim name args
  | mo == Cry.floatName   = compileCryptolFloatPrim name args
  | otherwise = unsupportedPrim (pp p) args


-- | Primitives defined in `Cryptol.cry`
compileCryptolPreludePrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolPreludePrim name args =
  case name of

    "False" -> pure (litExpr (boolLit False))
    "True"  -> pure (litExpr (boolLit True))

    "#" -> compilePrimAppend args

    -- Literal
    "number" ->
       pure $ mkRustCall (tyTraitMethod "number")
                         (primLenArgs args ++ primSizeArgs args)

    -- Zero
    "zero" ->
       pure $ mkRustCall (tyTraitMethod "zero") (primLenArgs args)


    -- Ring --
    "+"       -> inferMethod "add"
    "-"       -> inferMethod "sub"
    "*"       -> inferMethod "mul"
    "negate"  -> inferMethod "negate"


{-
    -- Integral --
    "!"       -> undefined
    "@"       -> undefined
-}

    _ -> pure (todoExp (Text.unpack name)) -- unsupportedPrim (pp name) args

{-
    "fromInteger" -> ring "from_integer"
    -- TODO: do we need to figure out if the exponent will fit in
    --       a u32 before calling this? (or call `Integral::to_usize`?)
    "^^" -> Nothing

    "/" -> integral "div"
    "%" -> integral "modulo"
    "toInteger" -> integral "to_integer"

    -- Zero
    "zero" -> Nothing -- TODO

    -- Logic
    "&&" -> logic "and"
    "||" -> logic "or"
    "^" -> logic "xor"
    "complement" -> logic "complement"
    _ -> Nothing
-}
  where
  inferMethod method =
    pure (mkRustCall (typeQualifiedExpr inferType (simplePath method))
                     (map addrOf (primArgs args)))

  tyTraitMethod method =
      case primTypeArgs args of
        [ty] -> typeQualifiedExpr ty (simplePath method)
        _    -> panic "tyTraitMethod" ["Expected exactly 1 type argument"]


-- | Primitives defined in `Float.cry`
compileCryptolFloatPrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolFloatPrim _ _ = unsupported "floating point primitve" -- XXX




--------------------------------------------------------------------------------

compilePrimAppend :: PrimArgs -> Rust RustExpr
compilePrimAppend args = pure (todoExp "#")
{-
  case primTypesOfArgs args of
    [ TWord (isKnownSize -> Just w1), TWord (isKnownSize -> Just w2) ] ->
      pure (callMacro (simplePath' [cryptolCrate,"append"])
                      (map (litExpr . mkIntLit Rust.Unsuffixed) [ w1, w2 ] ++ primArgs args))

    -- XXX: array + array, array + stream, word + stream

    _ -> unsupportedPrim "#" args
-}
