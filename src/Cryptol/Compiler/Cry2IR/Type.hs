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
                [ sz ] -> TIntegerMod <$> compileSizeType sz
                _      -> unexpected "Malformed TCIntMod"

            Cry.TCSeq ->
              case ts of
                [tlen,tel] ->
                  do sz <- compileStreamSizeType tlen
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
            Cry.TCAbstract {} -> unsupported "abstract value types"

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
    Cry.TNewtype {}     -> unsupported "newtype"    -- XXX

  where
  unexpected msg = panic "compileValType" [msg]


-- | Compile a known finite Cryptol size type to an IR type.
compileSizeType :: Cry.Type -> ConvertM Size
compileSizeType ty =
  do sz <- compileStreamSizeType ty
     case sz of
       IRSize s  -> pure s
       IRInfSize -> panic "compileSizeType" ["inf"]

-- | Compile a Cryptol size type to an IR type.
compileStreamSizeType :: Cry.Type -> ConvertM StreamSize
compileStreamSizeType ty =

  case ty of
    Cry.TUser _ _ t -> compileStreamSizeType t

    Cry.TVar t ->
      case t of
        Cry.TVBound v -> IRSize <$> lookupNumericTParam v
        Cry.TVFree {} -> unexpected "Free type variable"

    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum n       -> pure (IRSize (IRFixedSize n))
            Cry.TCInf         -> pure IRInfSize
            Cry.TCAbstract {} -> unsupported "abstract numeric types"

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
          do args <- mapM compileStreamSizeType ts
             pure (IRSize (evalSizeType tf args))

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNewtype {}     -> unexpected "TNewtype"
  where
  unexpected x = panic "compileStreamSizeType" [x]


