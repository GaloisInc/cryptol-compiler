-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.CompileType where

import qualified Language.Rust.Syntax as Rust

import Cryptol.Compiler.IR.Cryptol qualified as IR
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Error (unsupported)
import Cryptol.Compiler.Rust.Monad

-- compute the rust type used to represent the given cryptol type
compileType :: IR.Type -> Rust RustType
compileType ty =
  case ty of
    IR.TBool          -> pure boolType
    IR.TInteger       -> pure $ simpleType "Integer" -- XXX: num::BigInt
    IR.TIntegerMod _  -> pure $ simpleType "Integer"
    IR.TRational      -> pure $ simpleType "Rational"     -- XXX: ?
    IR.TFloat         -> pure $ simpleType "f32"
    IR.TDouble        -> pure $ simpleType "f64"

    IR.TWord sz ->
      case IR.isKnownSize sz of
        Just i -> pure $ fixedSizeWordType i
        Nothing -> unsupported "compile arbitrary sized word type"

    IR.TArray sz t ->
      case IR.isKnownSize sz of
        Just i  -> fixedArrayOfType <$> compileType t <*> pure i
        Nothing -> vectorOfType <$> compileType t

    IR.TStream _sz t  -> streamOfType <$> compileType t
    IR.TTuple ts      -> tupleType <$> compileType `traverse` ts
    IR.TPoly x        -> lookupTParam x
    IR.TFun args ret  -> funType <$> traverse compileType args
                                 <*> compileType ret


-- types
streamOfType :: RustType -> RustType
streamOfType = vectorOfType -- XXX

-- XXX
funType :: [RustType] -> RustType -> RustType
funType funArgs funRes = Rust.PathTy Nothing path ()
  where
  path = Rust.Path False [ Rust.PathSegment "Fun" (Just params) () ] ()
  params = Rust.AngleBracketed [] (funArgs ++ [funRes]) [] ()


fixedSizeWordType :: Integer -> RustType
fixedSizeWordType bits = Rust.MacTy mac ()
  where
      mac = Rust.Mac (simplePath' ["cryptol", "Word"]) tokenStream ()
      tokenStream = Rust.Tree lengthTok
      lengthTok = Rust.Token dummySpan $ Rust.LiteralTok (Rust.IntegerTok (show bits)) Nothing

