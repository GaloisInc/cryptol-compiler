module Cryptol.Compiler.Rust.CompileSize where

import Cryptol.Compiler.Error(panic,unsupported)

import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad

compileSizeType :: SizeVarSize -> RustType
compileSizeType szT =
  case szT of
    MemSize   -> simpleType "u64"
    LargeSize -> pathType (simplePath' ["num","BigUint"])

compileSize :: Size -> SizeVarSize -> Rust RustExpr
compileSize sz szT =
  case sz of
    IRFixedSize n ->
      case szT of
        MemSize   -> pure (litExpr (mkU64Lit n))
        LargeSize -> unsupported "large sizes"

    IRPolySize x
      | irsSize x == szT -> lookupSizeParam (irsName x)
      | otherwise -> panic "compileSize" [ "Size type mismatch" ]

    IRComputedSize {} -> unsupported "computed sizes"
    -- XXX

-- | Returning `Nothing` indicates `inf`
compileStreamSize :: StreamSize -> SizeVarSize -> Rust (Maybe RustExpr)
compileStreamSize = undefined



