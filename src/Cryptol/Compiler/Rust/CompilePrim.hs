module Cryptol.Compiler.Rust.CompilePrim where

import Cryptol.Compiler.Error (panic,unsupported)
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.CompileType(compileType)
import Cryptol.Compiler.Rust.CompileSize(compileSize)
import Cryptol.Compiler.Rust.CompileTrait (lenParamFor)

justExpr :: RustExpr -> ([RustStmt], RustExpr)
justExpr e = ([],e)

primNumber :: Call -> Rust ([RustStmt], RustExpr)
primNumber c =
  do  rustTy <- compileType ty
      case sizeType of

        -- `<rustTy>::number_u64()
        MemSize ->
          do  sizeExpr <- compileSize size sizeType
              let path = typePath rustTy (simplePath "number_u64")
              lenP <- lenParamFor ty
              pure $ justExpr (mkRustCall path [lenP, sizeExpr])

        LargeSize -> unsupported "primNumber: LargeSize not supported"

  where
  (ty, (size, sizeType)) =
    case ircFun c of
      IRTopFun tf | [t]   <- irtfTypeArgs tf
                  , [arg] <- irtfSizeArgs tf -> (t,arg)
      _ -> panic "primNumber" ["Unexpected ircFun"]

