module Cryptol.Compiler.Rust.CompileSize(compileSizeType, compileSize) where

import Data.Word(Word8)
import Data.Bits(shiftR)
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.EvalType

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad

compileSizeType :: ExprContext -> SizeVarSize -> RustType
compileSizeType ctxt szT =
  case szT of
    MemSize   -> simpleType "usize"
    LargeSize ->
      case ctxt of
        OwnContext -> own
        BorrowContext -> refType Nothing own
      where
      own = pathType (simplePath' ["num","BigUint"])


-- | Compile a size argument, using the specified type.
-- For big nums, this produces a reference.
compileSize :: ExprContext -> Size -> SizeVarSize -> Rust RustExpr
compileSize ctxt sz tgtSz =
  case sz of
    IRFixedSize n ->
      case tgtSz of
        MemSize   -> pure (litExpr (mkUSizeLit n))
        LargeSize -> pure (case ctxt of
                             OwnContext -> val
                             BorrowContext -> addrOf val)
          where
          val  =  mkRustCall fn [arg]
          fn   = pathExpr (simplePath' ["num","BigUint","from_bytes_le"])
          byte = litExpr . mkU8Lit . toInteger
          arg  = addrOf (rustArray (map byte (toLEBytes n)))

    IRPolySize x ->
      do p <- lookupSizeParam x
         pure (castSize (irsSize x) tgtSz p)

    IRComputedSize f as -> compileComputedSize f as tgtSz

compileComputedSize :: Cry.TFun -> [Size] -> SizeVarSize -> Rust RustExpr
compileComputedSize fu args sz =
  case fu of

    Cry.TCAdd ->
      withBin \x y ->
        do a <- compileSize BorrowContext x sz
           b <- compileSize BorrowContext y sz
           pure (op "add_size" [a,b])

    Cry.TCSub ->
      withBin \x y ->
        do let xsz = sizeTypeSize x
           a <- compileSize BorrowContext x xsz
           b <- compileSize BorrowContext y xsz
           pure (castSize xsz sz (op "sub_size" [a,b]))

    Cry.TCMul ->
      withBin \x y ->
        do a <- compileSize BorrowContext x sz
           b <- compileSize BorrowContext y sz
           pure (op "mul_size" [a,b])

    Cry.TCDiv ->
      withBin \x y ->
      do let use = memIfBoth x y
         a <- compileSize BorrowContext x use
         b <- compileSize BorrowContext y use
         pure (castSize use sz (op "div_size" [a,b]))

    Cry.TCMod ->
      withBin \x y ->
      do let use = memIfBoth x y
         a <- compileSize BorrowContext x use
         b <- compileSize BorrowContext y use
         pure (castSize use sz (op "mod_size" [a,b]))


    -- Assumes exponent fits in MemSize
    Cry.TCExp ->
      withBin \x y ->
      do a <- compileSize BorrowContext x sz
         b <- compileSize BorrowContext y MemSize
         pure (op "exp_size" [a,b])

    -- Assumes result fits in MemSize
    Cry.TCWidth ->
      withUn \x ->
        do a <- compileSize BorrowContext x (sizeTypeSize x)
           pure (castSize MemSize sz (op "width_size" [a]))

    Cry.TCMin ->
      withBin \x y ->
      do let use = memIfBoth x y
         a <- compileSize BorrowContext x use
         b <- compileSize BorrowContext y use
         pure (castSize use sz (op "min_size" [a,b]))

    Cry.TCMax ->
      withBin \x y ->
        do a <- compileSize BorrowContext x sz
           b <- compileSize BorrowContext y sz
           pure (op "max_size" [a,b])

    Cry.TCCeilDiv ->
      withBin \x y ->
      do let use = memIfBoth x y
         a <- compileSize BorrowContext x use
         b <- compileSize BorrowContext y use
         pure (castSize use sz (op "ceil_div_size" [a,b]))

    Cry.TCCeilMod ->
      withBin \x y ->
      do let use = memIfBoth x y
         a <- compileSize BorrowContext x use
         b <- compileSize BorrowContext y use
         pure (castSize use sz (op "ceil_mod_size" [a,b]))


    Cry.TCLenFromThenTo ->
      withThree \x y z ->
        do let use = case (sizeTypeSize x, sizeTypeSize y, sizeTypeSize z) of
                       (MemSize, MemSize, MemSize) -> MemSize
                       _ -> LargeSize
           a <- compileSize BorrowContext x use
           b <- compileSize BorrowContext x use
           c <- compileSize BorrowContext x use
           pure (castSize use sz (op "from_then_to_size" [a,b,c]))

  where
  bad = panic "compileComputedSize" ["Malformed expression"]


  memIfBoth x y =
    case (sizeTypeSize x, sizeTypeSize y) of
      (MemSize, MemSize) -> MemSize
      _                  -> LargeSize


  withUn f =
    case args of
      [x] -> f x
      _   -> bad

  withBin f =
    case args of
      [x,y] -> f x y
      _     -> bad

  withThree f =
    case args of
      [x,y,z] -> f x y z
      _       -> bad



-- | Convert a number to a list of bytes, least significant first.
toLEBytes :: Integer -> [Word8]
toLEBytes n =
  case compare n 0 of
    LT -> panic "toLEbytes" ["negative"]
    EQ -> []
    GT -> fromInteger n : toLEBytes (n `shiftR` 8)

castSize :: SizeVarSize -> SizeVarSize -> RustExpr -> RustExpr
castSize from to expr =
  case (from,to) of
    (MemSize, LargeSize)  -> op "size_to_int" [expr]
    (LargeSize,MemSize)   -> op "int_to_size" [expr]
    _                     -> expr


op :: Rust.Ident -> [RustExpr] -> RustExpr
op = mkRustCall . pathExpr . simplePath


