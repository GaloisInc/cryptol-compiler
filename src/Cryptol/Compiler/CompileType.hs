module Cryptol.Compiler.CompileType where

import qualified Cryptol.TypeCheck.AST as Cry
import qualified Cryptol.TypeCheck.Solver.InfNat as Cry

import Cryptol.Compiler.IR
import Cryptol.Compiler.Monad

{- | Maps a Cryptol type to an IR type.
Assumptions:
  (1) Size polymorphic type variables are finite
  (2) Value polymorphic teyp variables are not Bit

The idea is that we generate specialized versions for polymorphic variables
that can scope over those cases.
-}
compileValType :: Cry.Type -> CryC Type
compileValType ty =
  case ty of
    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum {}         -> unexpected "TCNum"
            Cry.TCInf            -> unexpected "TCInf"

            Cry.TCBit            -> pure TBool
            Cry.TCInteger        -> pure TInteger
            Cry.TCRational       -> pure TRational

            Cry.TCFloat ->
              case ts of
                [ e, p ] ->
                  do ce <- compileStreamSizeType e
                     cp <- compileStreamSizeType p
                     case (ce,cp) of
                       (IRSize (IRFixedSize pr), IRSize (IRFixedSize ex))
                          | pr == 8  && ex == 24 -> pure TFloat
                          | pr == 11 && ex == 53 -> pure TDouble
                       _ -> unsupported "floating point type"
                _ -> unexpected "Malformed TFCFloat"

            Cry.TCIntMod ->
              case ts of
                [ sz ] ->
                  do csz <- compileStreamSizeType sz
                     case csz of
                       IRSize s  -> pure (TIntegerMod s)
                       IRInfSize -> unexpected "TCIntMod Inf"
                _ -> unexpected "Malformed TCIntMod"


            Cry.TCSeq ->
              case ts of
                [tlen,tel] ->
                  do isize <- compileStreamSizeType tlen
                     vt    <- compileValType tel
                     case isize of
                       IRInfSize -> pure (TStream isize vt)
                       IRSize sz ->
                         case vt of
                           TBool -> pure (TWord sz)
                           _     -> pure (TArray sz vt)

                _ -> unexpected "Malformed TSeq"

            Cry.TCTuple {}  -> TTuple <$> mapM compileValType ts

            Cry.TCFun         -> unsupported "higher order functions"
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
    Cry.TRec {}         -> unsupported "records"    -- XXX
    Cry.TNewtype {}     -> unsupported "newtype"    -- XXX

  where
  unexpected msg = panic "compileValType" [msg]


-- | Compile a Cryptol size type to an IR type.
compileStreamSizeType :: Cry.Type -> CryC StreamSize
compileStreamSizeType ty =
  case ty of
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
             let dflt = IRSize (IRComputedSize tf args)
             pure case tf of
                    Cry.TCAdd           -> liftInfNat dflt args Cry.nAdd
                    Cry.TCSub           -> liftInfNat dflt args Cry.nSub
                    Cry.TCMul           -> liftInfNat dflt args Cry.nMul
                    Cry.TCDiv           -> liftInfNat dflt args Cry.nDiv
                    Cry.TCMod           -> liftInfNat dflt args Cry.nMod
                    Cry.TCExp           -> liftInfNat dflt args Cry.nExp
                    Cry.TCWidth         -> liftInfNat dflt args Cry.nWidth
                    Cry.TCMin           -> liftInfNat dflt args Cry.nMin
                    Cry.TCMax           -> liftInfNat dflt args Cry.nMax
                    Cry.TCCeilDiv       -> liftInfNat dflt args Cry.nCeilDiv
                    Cry.TCCeilMod       -> liftInfNat dflt args Cry.nCeilMod
                    Cry.TCLenFromThenTo ->
                       liftInfNat dflt args Cry.nLenFromThenTo

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TVar t          -> case t of
                             Cry.TVBound v -> pure (IRSize (IRPolySize v))
                             Cry.TVFree {} -> unexpected "TVFree"
    Cry.TUser _ _ t     -> compileStreamSizeType t

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNewtype {}     -> unexpected "TNewtype"
  where
  unexpected x = panic "compileStreamSizeType" [x]


class LiftInfNat a where
  liftInfNat :: StreamSize -> [StreamSize] -> a -> StreamSize

instance LiftInfNat a => LiftInfNat (Maybe a) where
  liftInfNat dflt xs a =
    case a of
      Just v  -> liftInfNat dflt xs v
      Nothing -> panic "liftInfNat" ["Malformed size type: partial"]

instance LiftInfNat Cry.Nat' where
  liftInfNat _ xs a =
    case xs of
      [] ->
        case a of
          Cry.Inf   -> IRInfSize
          Cry.Nat n -> IRSize (IRFixedSize n)
      _  -> panic "liftInfNat" ["Malformed size type: extra arguments"]

instance LiftInfNat a => LiftInfNat (Cry.Nat' -> a) where
  liftInfNat dflt xs f =
    case xs of
      y : ys ->
        case y of
          IRInfSize              -> liftInfNat dflt ys (f Cry.Inf)
          IRSize (IRFixedSize i) -> liftInfNat dflt ys (f (Cry.Nat i))
          _                      -> dflt

      [] -> panic "liftInfNat" ["Malformed size type: missing arguments"]

