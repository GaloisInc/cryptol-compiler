module Cryptol.Compiler.CompileType where

import qualified Cryptol.TypeCheck.AST as Cry
import qualified Cryptol.TypeCheck.Solver.InfNat as Cry

import Cryptol.Compiler.IR
import Cryptol.Compiler.Monad

compileValType :: Cry.Type -> CryC Type
compileValType ty =
  case ty of
    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum {}         -> panic "compileValType" ["TCNum"]
            Cry.TCInf            -> panic "compileValType" ["TCInf"]
            Cry.TCBit            -> pure TBool

            Cry.TCInteger        -> unsupported "Integer type"
            Cry.TCFloat          -> unsupported "Floating point type"
            Cry.TCIntMod         -> unsupported "Z type"
            Cry.TCRational       -> unsupported "Rational type"
            Cry.TCArray          -> unsupported "Array type"

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

                _ -> panic "compileValType" ["Malformed TSeq"]

            Cry.TCFun       -> unsupported "higher order functions"
            Cry.TCTuple {}  -> TTuple <$> mapM compileValType ts

            Cry.TCAbstract {} -> unsupported "abstract value types"

        Cry.PC {}       -> panic "compileValType" ["PC"]
        Cry.TF {}       -> panic "compileValType" ["TF"]
        Cry.TError {}   -> panic "compileValType" ["TError"]

    Cry.TVar t ->
      case t of
        Cry.TVBound a -> pure (TPoly a)
        Cry.TVFree {} -> panic "compileValType" ["TVFree"]

    Cry.TUser _ _ t     -> compileValType t
    Cry.TRec {}         -> unsupported "records"
    Cry.TNewtype {}     -> unsupported "newtype"


-- | Compile a Cryptol size type to an IR type.
compileStreamSizeType :: Cry.Type -> CryC (IRStreamSize Cry.TParam)
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
             undefined -- pure (IRSize (IRComputedSize tf args))

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


