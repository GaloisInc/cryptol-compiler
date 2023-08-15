module Cryptol.Compiler.Rust.CompilePrim where

import Data.Text(Text)
import Data.Text qualified as Text
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.Error (panic,unsupported)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.CompileType

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

unsupportedPrim :: Doc -> PrimArgs -> a
unsupportedPrim nm args =
  unsupported (Text.pack (show (vcat [ "primitive" <+> nm, pp args ])))


-- | Is this a constructor primitive.
-- If so, it takes ownership of its arguments.
primIsConstructor :: IRPrim -> Bool
primIsConstructor prim =
  case prim of
    CryPrim {}  -> False

    MakeSeq     -> True
    ArrayLookup -> False

    Tuple       -> True
    TupleSel {} -> False

    EqSize      -> False
    LeqSize     -> False

    Map         -> notYet
    FlatMap     -> notYet
    Zip         -> notYet
    Collect     -> notYet
    Iter        -> notYet

  where
  notYet = unsupported (Text.pack (show ("primitive" <+> pp prim)))

-- | Emit code for a primitve.
compilePrim :: IRPrim -> PrimArgs -> Rust RustExpr
compilePrim name args =
  case name of
    CryPrim p -> compileCryptolPrim p args

    MakeSeq   -> compileSeqLit args

    _         -> unsupportedPrim (pp name) args


compileCryptolPrim :: Cry.PrimIdent -> PrimArgs -> Rust RustExpr
compileCryptolPrim p@(Cry.PrimIdent mo name) args
  | mo == Cry.preludeName = compileCryptolPreludePrim name args
  | mo == Cry.floatName   = compileCryptolFloatPrim name args
  | otherwise = unsupportedPrim (pp p) args


-- | Primitives defined in `Cryptol.cry`
compileCryptolPreludePrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolPreludePrim name args =
  case name of

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



    _ -> unsupportedPrim (pp name) args

{-
    "fromInteger" -> ring "from_integer"
    -- TODO: do we need to figure out if the exponent will fit in
    --       a u32 before calling this? (or call `Integral::to_usize`?)
    "^^" -> Nothing

    -- Integral --
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
    pure (mkRustCall (typePath inferType (simplePath method))
                     (map addrOf (primArgs args)))

  tyTraitMethod method =
      case primTypeArgs args of
        [ty] -> typePath ty (simplePath method)
        _    -> panic "tyTraitMethod" ["Expected exactly 1 type argument"]


-- | Primitives defined in `Float.cry`
compileCryptolFloatPrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolFloatPrim = unsupported "floating point primitve" -- XXX




--------------------------------------------------------------------------------
compileSeqLit :: PrimArgs -> Rust RustExpr
compileSeqLit args =
  case primTypeOfResult args of

    TArray sz elTy ->
      case isKnownSize sz of
        Just n  ->
          do arrTy <- cryArrayType n <$> compileType AsOwned elTy
             pure (mkRustCall (typePath arrTy (simplePath "from"))
                              [ arrayExpr (primArgs args) ])
        Nothing -> unsupportedPrim "vec" args



    -- TWord sz
    -- TStream sz _elTy

    _ -> unsupportedPrim "MakeSeq" args
