module Cryptol.Compiler.Rust.CompilePrim where

import Cryptol.Compiler.Error (panic,unsupported)
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Types (compileType)
import Cryptol.Compiler.Rust.CompileSize(compileSize)

primNumber :: Call -> Rust RustExpr
primNumber c =
  do  rustTy <- compileType ty
      case sizeType of

        -- `<rustTy>::number_u64()
        MemSize ->
          do  sizeExpr <- compileSize size sizeType
              let path = typePath rustTy (simplePath "number_u64")
              pure $ mkRustCall path [unitExpr, sizeExpr]

        LargeSize -> unsupported "primNumber: LargeSize not supported"

  where
  (ty, (size, sizeType)) =
    case ircFun c of
      IRTopFun tf | [t]   <- irtfTypeArgs tf
                  , [arg] <- irtfSizeArgs tf -> (t,arg)
      _ -> panic "primNumber" ["Unexpected ircFun"]

