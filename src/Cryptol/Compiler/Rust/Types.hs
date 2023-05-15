-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.Types where

import qualified Language.Rust.Syntax as Rust
import Language.Rust.Data.Ident(mkIdent)

import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.IR.Cryptol qualified as IR
import Cryptol.Compiler.Rust.Utils

-- types
unitType :: RustType
unitType = tupleType []

rustSimpleType :: String -> RustType
rustSimpleType i = Rust.PathTy Nothing path ()
  where
    path = Rust.Path True [Rust.PathSegment ident Nothing ()] ()
    ident = mkIdent i

streamOfType :: RustType -> RustType
streamOfType = vectorOfType -- XXX

vectorOfType :: RustType -> RustType
vectorOfType elT = Rust.PathTy Nothing path ()
  where
  path = Rust.Path False [ Rust.PathSegment "Vec" (Just params) () ] ()
  params = Rust.AngleBracketed [] [elT] [] ()

-- XXX
funType :: [RustType] -> RustType -> RustType
funType funArgs funRes = Rust.PathTy Nothing path ()
  where
  path = Rust.Path False [ Rust.PathSegment "Fun" (Just params) () ] ()
  params = Rust.AngleBracketed [] (funArgs ++ [funRes]) [] ()


fixedArrayOfType :: Rust.Ty () -> Integer -> Rust.Ty ()
fixedArrayOfType ty i = Rust.Array ty sizeExpr ()
  where
    sizeExpr = Rust.Lit [] (Rust.Int Rust.Dec i Rust.Unsuffixed ()) ()

tupleType :: [Rust.Ty ()] -> Rust.Ty ()
tupleType tys = Rust.TupTy tys ()

computeFixedSize :: IR.Size -> Maybe Integer
computeFixedSize sz = case sz of
  IR.IRFixedSize n -> Just n
  IR.IRPolySize tp -> Nothing
  -- TODO: compute
  IR.IRComputedSize tf isss -> Nothing

-- compute the rust type used to represent the given cryptol type
rustRep :: (?poly :: Cry.TParam -> RustType) => IR.Type -> RustType
rustRep ty =
  case ty of
    IR.TBool -> bool
    IR.TInteger -> rustSimpleType "Integer"
    IR.TIntegerMod _ -> rustSimpleType "Integer"
    IR.TRational -> rustSimpleType "Rational"
    IR.TFloat -> rustSimpleType "f32"
    IR.TDouble -> rustSimpleType "f64"
    IR.TWord sz ->
      case computeFixedSize sz of
        Just i
          | i == 0 -> unitType
          | i == 1 -> bool
          | i <= 8 -> rustSimpleType "u8"
          | i <= 16 -> rustSimpleType "u16"
          | i <= 32 -> rustSimpleType "u32"
          | i <= 64 -> rustSimpleType "u64"
        _ -> rustSimpleType "BitVector"
    -- IR.TSize -> rustSimpleType "usize"  -- is this right?
    IR.TArray sz t ->
      case computeFixedSize sz of
        Just i -> fixedArrayOfType (rustRep t) i
        Nothing -> vectorOfType (rustRep t)
    IR.TStream sz t -> streamOfType (rustRep t)
    IR.TTuple ts -> tupleType (rustRep <$> ts)
    IR.TPoly x -> ?poly x
    IR.TFun args ret -> funType (map rustRep args) (rustRep ret)
  where
    bool = rustSimpleType "bool"

