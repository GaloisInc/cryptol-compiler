module Cryptol.Compiler.Rust.CompilePrim where

import Data.Text(Text)
import Data.Text qualified as Text
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.Error (panic,unsupported)
import Cryptol.Compiler.PP(pp)
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils

data PrimArgs = PrimArgs
  { primTypeArgs  :: [RustType]
  , primLenArgs   :: [RustExpr]
  , primSizeArgs  :: [RustExpr]
  , primArgs      :: [RustExpr]
  }

-- | Emit code for a primitve.
compilePrim :: IRPrim -> PrimArgs -> Rust RustExpr
compilePrim name args =
  case name of
    CryPrim p -> compileCryptolPrim p args
    _ -> unsupported (Text.pack ("primitive " ++ show (pp name))) -- XXX


compileCryptolPrim :: Cry.PrimIdent -> PrimArgs -> Rust RustExpr
compileCryptolPrim p@(Cry.PrimIdent mo name) args
  | mo == Cry.preludeName = compileCryptolPreludePrim name args
  | mo == Cry.floatName   = compileCryptolFloatPrim name args
  | otherwise =
    unsupported ("Cryptol Prim " <> Text.pack (show (pp p)))


-- | Primitives defined in `Cryptol.cry`
compileCryptolPreludePrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolPreludePrim name args =
  case name of
    "number" ->
       pure $ mkRustCall (tyTraitMethod "number")
                         (primLenArgs args ++ primSizeArgs args)

    "zero" ->
       pure $ mkRustCall (tyTraitMethod "zero") (primLenArgs args)



    _ -> unsupported ("Primitive " <> name)

{-
      -- Ring --
    "+" -> ring "add"
    "-" -> ring "sub"
    "*" -> ring "mul"
    "negate" -> ring "negate"
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
  ring     = mkTraitFnExpr "Ring"
  integral = mkTraitFnExpr "Integral"
  logic    = mkTraitFnExpr "Logic"
  mkTraitFnExpr trait method = pathExpr (simplePath' [trait, method])

  tyTraitMethod method =
      case primTypeArgs args of
        [ty] -> typePath ty (simplePath method)
        _    -> panic "tyTraitMethod" ["Expected exactly 1 type argument"]


-- | Primitives defined in `Float.cry`
compileCryptolFloatPrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolFloatPrim = unsupported "floating point primitve" -- XXX

