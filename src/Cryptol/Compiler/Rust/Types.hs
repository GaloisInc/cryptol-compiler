-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.Types where

import qualified Language.Rust.Syntax as Rust

import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.IR.Cryptol qualified as IR
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Error (unsupported)
import Cryptol.Compiler.Rust.Monad

type TypeParams = (?poly :: Cry.TParam -> RustType)

-- compute the rust type used to represent the given cryptol type
compileType :: IR.Type -> Rust RustType
compileType ty =
  case ty of
    IR.TBool          -> pure boolType
    IR.TInteger       -> pure $ rustSimpleType "Integer"
    IR.TIntegerMod _  -> pure $ rustSimpleType "Integer"
    IR.TRational      -> pure $ rustSimpleType "Rational"
    IR.TFloat         -> pure $ rustSimpleType "f32"
    IR.TDouble        -> pure $ rustSimpleType "f64"

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



