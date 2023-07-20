module Cryptol.Compiler.Rust.CompilePrim where
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Error (panic)
import Cryptol.Compiler.Rust.Types (compileType)
import Cryptol.Compiler.Cry2IR.Specialize (compileSizeType)

primNumber :: Call -> Rust RustExpr
primNumber c =
  do  rustTy <- compileType ty
      case sizeType of

        -- `<rustTy>::number_u64()
        MemSize ->
          do  sizeExpr <- compileSizeType size sizeType
              let path = pathExpr $ typePath rustTy (simplePath "number_u64")
              pure $ mkRustCall path [unitExpr, sizeExpr]

        LargeSize -> unsupported "primNumber: LargeSize not supported"

  where
  (size, sizeType) =
    case ircFun c of
      IRTopFun tf | [arg] <- irtfSizeArgs tf -> arg
      IRFunVal _ -> panic "primNumber" ["Unexpected ircFun"]
  ty =
    case ircArgTypes c of
      [t] -> t
      _ -> panic "primNumber" ["Unexpected type args"]
