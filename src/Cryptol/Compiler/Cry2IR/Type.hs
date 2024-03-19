module Cryptol.Compiler.Cry2IR.Type where

import Cryptol.Utils.RecordMap qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.Cry2IR.ConvertM
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.IR.Cryptol

-- | This will only use a stream for infinite ones.
compileValType :: Cry.Type -> ConvertM Type
compileValType ty =
  case ty of
    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum {}   -> unexpected "TCNum"
            Cry.TCInf      -> unexpected "TCInf"

            Cry.TCBit      -> pure TBool
            Cry.TCInteger  -> pure TInteger
            Cry.TCRational -> pure TRational

            Cry.TCFloat ->
              case ts of
                [ e, p ] ->
                   case (Cry.tIsNum e, Cry.tIsNum p) of
                     (Just 8,  Just 24) -> pure TFloat
                     (Just 11, Just 53) -> pure TDouble
                     _ -> unsupported "Floating point size"
                _ -> unsupported "Malformed TCFloat"

            Cry.TCIntMod ->
              case ts of
                [ sz ] -> TIntegerMod <$> compileSizeType False sz
                _      -> unexpected "Malformed TCIntMod"

            Cry.TCSeq ->
              case ts of
                [tlen,tel] ->
                  do sz <- compileStreamSizeType True tlen
                     el <- compileValType tel
                     case sz of
                       IRInfSize -> pure (TStream IRInfSize el)
                       IRSize len ->
                         case el of
                           TBool -> pure (TWord len)
                           _     -> pure (TArray len el)
                _ -> unexpected "Malformed TSeq"

            Cry.TCTuple {}    -> TTuple <$> mapM compileValType ts

            Cry.TCFun ->
              case ts of
                [a,b] ->
                  do let (as,c) = Cry.splitWhile Cry.tIsFun b
                     args <- mapM compileValType (a:as)
                     res  <- compileValType c
                     pure (TFun args res)
                _ -> unexpected "Malformed function type"

            Cry.TCArray       -> unsupported "Array type"

        Cry.PC {}       -> unexpected "PC"
        Cry.TF {}       -> unexpected "TF"
        Cry.TError {}   -> unexpected "TError"

    Cry.TVar t ->
      case t of
        Cry.TVBound a -> pure (TPoly a)
        Cry.TVFree {} -> unexpected "TVFree"

    Cry.TUser _ _ t     -> compileValType t
    Cry.TRec rec ->
      TTuple <$> mapM compileValType (Cry.recordElements rec)
    Cry.TNominal {}     -> unsupported "nominal type"    -- XXX

  where
  unexpected msg = panic "compileValType" [msg]


-- | Compile a known finite Cryptol size type to an IR type.
compileSizeType :: Bool -> Cry.Type -> ConvertM Size
compileSizeType forceSmall ty =
  do sz <- compileStreamSizeType forceSmall ty
     case sz of
       IRSize s  -> pure s
       IRInfSize -> panic "compileSizeType" ["inf"]

-- | Compile a Cryptol size type to an IR type.
compileStreamSizeType :: Bool -> Cry.Type -> ConvertM StreamSize
compileStreamSizeType forceSmall ty =

  case ty of
    Cry.TUser _ _ t -> compileStreamSizeType forceSmall t

    Cry.TVar t ->
      case t of
        Cry.TVBound v -> lookupNumericTParam v
        Cry.TVFree {} -> unexpected "Free type variable"

    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum n       -> pure (IRSize (IRFixedSize n))
            Cry.TCInf         -> pure IRInfSize

            Cry.TCBit         -> unexpected "TCBit"
            Cry.TCInteger     -> unexpected "TCInteger"
            Cry.TCFloat       -> unexpected "TCFloat"
            Cry.TCIntMod      -> unexpected "TCIntMod"
            Cry.TCRational    -> unexpected "TCRational"
            Cry.TCArray       -> unexpected "TCArray"
            Cry.TCSeq         -> unexpected "TCSeq"
            Cry.TCFun         -> unexpected "TCFun"
            Cry.TCTuple {}    -> unexpected "TCTuple"


        Cry.TF tf ->
          do args <- mapM (compileStreamSizeType False) ts
             rsz  <- getTypeSize forceSmall ty
             pure (IRSize (evalSizeType tf args rsz))

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNominal {}     -> unexpected "TNominal"
  where
  unexpected x = panic "compileStreamSizeType" [x]


