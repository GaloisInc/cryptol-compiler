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
compileSize sz tgtSz =
  case sz of
    IRFixedSize n ->
      case tgtSz of
        MemSize   -> pure (litExpr (mkU64Lit n))
        LargeSize -> pure (addrOf (mkRustCall fn [arg]))
          where
          fn   = pathExpr (simplePath' ["num","BigUint","from_bytes_le"])
          byte = litExpr . mkU8Lit . toInteger
          arg  = addrOf (rustArray (map byte (toLEBytes n)))

    IRPolySize x ->
      do p <- lookupSizeParam (irsName x)
         case (irsSize x, tgtSz) of
           (MemSize, LargeSize) -> undefined -- upcast
           (LargeSize, MemSize) -> undefined -- downcast, dyn check
           _                    -> pure p

    IRComputedSize f as -> compileComputedSize f as tgtSz

compileComputedSize :: Cry.TFun -> [Size] -> SizeVarSize -> Rust RustExpr
compileComputedSize fu args sz = undefined
{-
  case fu of
    Cry.TCAdd ->
      withBin \x y ->
        do a <- compileSize x sz
           b <- compileSize y sz
           pure (op2 "addSize" a b)

    Cry.TCSub ->
      withBin \x y ->
        do let xsz = sizeTypeSize x
           a <- compileSize x xsz
           b <- compileSize y xsz
           pure (op2 "subSize" a b)

    Cry.TCMul ->
      withBin \x y ->
        do a <- compileSize x sz
           b <- compileSize y sz
           pure (op2 "mulSize" a b)

    Cry.TCDiv ->
      withBin \x y ->
      do let xsz = sizeTypeSize x
         a <- compileSize x xsz
         b <- compileSize y xsz
         pure (op2 "divSize" a b)

    Cry.TCMod ->
      withBin \x y ->
      do let xsz = sizeTypeSize x
         a <- compileSize x xsz
         b <- compileSize y xsz
         pure (op2 "divSize" a b)



    _ -> undefined
-}
{-
    Cry.TCMod           -> undefined  
    Cry.TCExp           -> undefined (inf ^ x) -> 1
    Cry.TCWidth         -> undefined
    Cry.TCMin           -> undefined
    Cry.TCMax           -> undefined
    Cry.TCCeilDiv       -> undefined
    Cry.TCCeilMod       -> undefined
    Cry.TCLenFromThenTo -> undefined
-}

  where
  bad = panic "compileComputedSize" ["Malformed expression"]

  op2 = undefined

  withBin f =
    case args of
      [x,y] -> f x y
      _     -> bad


-- | Convert a number to a list of bytes, least significant first.
toLEBytes :: Integer -> [Word8]
toLEBytes n =
  case compare n 0 of
    LT -> panic "toLEbytes" ["negative"]
    EQ -> []
    GT -> fromInteger n : toLEBytes (n `shiftR` 8)



