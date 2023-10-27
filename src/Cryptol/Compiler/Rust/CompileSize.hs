module Cryptol.Compiler.Rust.CompileSize where

import Data.Word(Word8)
import Data.Bits(shiftR)

import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad

compileSizeType :: SizeVarSize -> RustType
compileSizeType szT =
  case szT of
    MemSize   -> simpleType "u64"
    LargeSize -> refType (pathType (simplePath' ["num","BigUint"]))

-- | Compile a size argument, using the specified type.
-- For big nums, this produces a reference.
compileSize :: Size -> SizeVarSize -> Rust RustExpr
compileSize sz szT =
  case sz of
    IRFixedSize n ->
      case szT of
        MemSize   -> pure (litExpr (mkU64Lit n))
        LargeSize -> pure (addrOf (mkRustCall fn [arg]))
          where
          fn   = pathExpr (simplePath' ["num","BigUint","from_bytes_le"])
          byte = litExpr . mkU8Lit . toInteger
          arg  = addrOf (rustArray (map byte (toLEBytes n)))

    IRPolySize x
      | irsSize x == szT -> lookupSizeParam (irsName x)
      | otherwise -> panic "compileSize" [ "Size type mismatch" ]

    IRComputedSize f as -> compileComputedSize szT f as

compileComputedSize :: SizeVarSize -> Cry.TFun -> [Size] -> Rust RustExpr
compileComputedSize sz fu args = unsupported "computed size" {- XXX
  case fu of
    Cry.TCAdd ->
      do xs <- mapM (\x -> compileSize (streamSizeToSize x) sz) args
         undefined

    Cry.TCSub           -> undefined  (fin,fin)
    Cry.TCMul           -> undefined  (inf * x) -> 0
    Cry.TCDiv           -> undefined  fin fin
    Cry.TCMod           -> undefined  
    Cry.TCExp           -> undefined (inf ^ x) -> 1
    Cry.TCWidth         -> undefined
    Cry.TCMin           -> undefined
    Cry.TCMax           -> undefined
    Cry.TCCeilDiv       -> undefined
    Cry.TCCeilMod       -> undefined
    Cry.TCLenFromThenTo -> undefined

-}



-- | Convert a number to a list of bytes, least significant first.
toLEBytes :: Integer -> [Word8]
toLEBytes n =
  case compare n 0 of
    LT -> panic "toLEbytes" ["negative"]
    EQ -> []
    GT -> fromInteger n : toLEBytes (n `shiftR` 8)



