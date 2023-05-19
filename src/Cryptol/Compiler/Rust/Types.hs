-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.Types where

import qualified Language.Rust.Syntax as Rust

import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.IR.Cryptol qualified as IR
import Cryptol.Compiler.Rust.Utils

-- compute the rust type used to represent the given cryptol type
rustRep :: (?poly :: Cry.TParam -> RustType) => IR.Type -> RustType
rustRep ty =
  case ty of
    IR.TBool          -> boolType
    IR.TInteger       -> rustSimpleType "Integer"
    IR.TIntegerMod _  -> rustSimpleType "Integer"
    IR.TRational      -> rustSimpleType "Rational"
    IR.TFloat         -> rustSimpleType "f32"
    IR.TDouble        -> rustSimpleType "f64"

    IR.TWord sz ->
      case IR.isKnownSize sz of
        Just i
          | i == 0    -> unitType
          | i == 1    -> boolType
          | i == 8    -> rustSimpleType "u8"
          | i == 16   -> rustSimpleType "u16"
          | i == 32   -> rustSimpleType "u32"
          | i == 64   -> rustSimpleType "u64"
          | i == 128  -> rustSimpleType "u128"
        _ -> rustSimpleType "BitVector"

    IR.TArray sz t ->
      case IR.isKnownSize sz of
        Just i  -> fixedArrayOfType (rustRep t) i
        Nothing -> vectorOfType (rustRep t)

    IR.TStream _sz t  -> streamOfType (rustRep t)
    IR.TTuple ts      -> tupleType (rustRep <$> ts)
    IR.TPoly x        -> ?poly x
    IR.TFun args ret  -> funType (map rustRep args) (rustRep ret)


-- types
streamOfType :: RustType -> RustType
streamOfType = vectorOfType -- XXX

-- XXX
funType :: [RustType] -> RustType -> RustType
funType funArgs funRes = Rust.PathTy Nothing path ()
  where
  path = Rust.Path False [ Rust.PathSegment "Fun" (Just params) () ] ()
  params = Rust.AngleBracketed [] (funArgs ++ [funRes]) [] ()



