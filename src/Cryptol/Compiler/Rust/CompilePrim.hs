module Cryptol.Compiler.Rust.CompilePrim where

import Cryptol.Compiler.Error (panic,unsupported)
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.CompileType(compileType)
import Cryptol.Compiler.Rust.CompileSize(compileSize)



primNumber :: Call -> Rust RustExpr
primNumber c =
  do  rustTy <- compileType ty
      case sizeType of

        -- `<rustTy>::number_u64()
        MemSize ->
          do  sizeExpr <- compileSize size sizeType
              let path = typePath rustTy (simplePath "number_u64")
              lenP <- lenParamFor ty
              pure $ mkRustCall path [lenP, sizeExpr]

        LargeSize -> unsupported "primNumber: LargeSize not supported"

  where
  (ty, (size, sizeType)) =
    case ircFun c of
      IRTopFun tf | [t]   <- irtfTypeArgs tf
                  , [arg] <- irtfSizeArgs tf -> (t,arg)
      _ -> panic "primNumber" ["Unexpected ircFun"]


lenParamFor :: Type -> Rust RustExpr
lenParamFor ty =
  case ty of
    TBool                     -> pure unitExpr
    TInteger                  -> pure unitExpr
    TIntegerMod _             -> pure unitExpr
    TRational                 -> pure unitExpr
    TFloat                    -> pure unitExpr
    TDouble                   -> pure unitExpr
    TWord sz | Just _ <- isKnownSize sz -> pure unitExpr
             | otherwise                -> compileSize sz MemSize

    TArray sz elT
      | Just _ <- isKnownSize sz  -> lenParamFor elT
      | otherwise ->
        do vecLen  <- compileSize sz MemSize
           elemLen <- lenParamFor elT
           pure (tupleExpr [vecLen,elemLen])

    TStream {} -> unsupported "lenStream"
    TTuple ts       -> tupleExpr <$> mapM lenParamFor ts
    TFun _ t        -> lenParamFor t
    TPoly tp        -> lookupLenParam tp

lenParamForSize :: Size -> Rust RustExpr
lenParamForSize = undefined

