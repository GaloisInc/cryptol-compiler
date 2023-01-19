module Cryptol.Compiler.CompileType where

import qualified Cryptol.TypeCheck.AST as Cry
import qualified Cryptol.TypeCheck.Solver.InfNat as Cry

import Cryptol.Compiler.IR
import Cryptol.Compiler.Monad

compileValType :: Cry.Type -> CryC IRType
compileValType ty =
  case ty of
    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum {}         -> panic "compileValType" ["TCNum"]
            Cry.TCInf            -> panic "compileValType" ["TCInf"]
            Cry.TCBit            -> pure TBool

            -- Some of these we'll support eventually
            Cry.TCInteger        -> unsupported "Integer type"
            Cry.TCFloat          -> unsupported "Floating point type"
            Cry.TCIntMod         -> unsupported "Z type"
            Cry.TCRational       -> unsupported "Rational type"
            Cry.TCArray          -> unsupported "Array type"

            Cry.TCSeq ->
              case ts of
                [tlen,tel] ->
                  do mbsz <- compileSizeType tlen
                     vt <- compileValType tel
                     case mbsz of
                       Nothing -> pure (TStream Nothing vt)
                       Just sz ->
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

    Cry.TVar _          -> unsupported "polymorphic value type" -- XXX
    Cry.TUser _ _ t     -> compileValType t
    Cry.TRec {}         -> unsupported "records"
    Cry.TNewtype {}     -> unsupported "newtype"


-- | Compile a Cryptol size type to an IR type.
-- 'Nothing' means "infinity" (i.e., an unbounded stream).
-- If the type depends on type varible, then the size is `IRUnknown`
compileSizeType :: Cry.Type -> CryC (Maybe IRSize)
compileSizeType ty =
  do mb <- sizeTypeToNat' ty
     case mb of
       Nothing -> pure (Just IRUnknown)
       Just cn ->
         case cn of
           Cry.Inf -> pure Nothing
           Cry.Nat n
             | n > toInteger (maxBound :: Int) ->
               unsupported "Size type is too large"
             | otherwise -> pure (Just (IRFixed (fromIntegral n)))


-- | Compute a size type.
-- Returns 'Nothing' if the size depends on a type variable.
sizeTypeToNat' :: Cry.Type -> CryC (Maybe Cry.Nat')
sizeTypeToNat' ty =
  case ty of
    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum n       -> pure (Just (Cry.Nat n))
            Cry.TCInf         -> pure (Just Cry.Inf)
            Cry.TCAbstract {} -> unsupported "abstract numeric types"

            Cry.TCBit         -> panic "sizeTypeToNat'" ["TCBit"]
            Cry.TCInteger     -> panic "sizeTypeToNat'" ["TCInteger"]
            Cry.TCFloat       -> panic "sizeTypeToNat'" ["TCFloat"]
            Cry.TCIntMod      -> panic "sizeTypeToNat'" ["TCIntMod"]
            Cry.TCRational    -> panic "sizeTypeToNat'" ["TCRational"]
            Cry.TCArray       -> panic "sizeTypeToNat'" ["TCArray"]
            Cry.TCSeq         -> panic "sizeTypeToNat'" ["TCSeq"]
            Cry.TCFun         -> panic "sizeTypeToNat'" ["TCFun"]
            Cry.TCTuple {}    -> panic "sizeTypeToNat'" ["TCTuple"]


        Cry.TF tf ->
          do args <- mapM sizeTypeToNat' ts
             pure
               case sequence args of
                 Nothing -> Nothing
                 Just vs ->
                   Just
                     case tf of
                       Cry.TCAdd           -> liftEvalType Cry.nAdd vs
                       Cry.TCSub           -> liftEvalType Cry.nSub vs
                       Cry.TCMul           -> liftEvalType Cry.nMul vs
                       Cry.TCDiv           -> liftEvalType Cry.nDiv vs
                       Cry.TCMod           -> liftEvalType Cry.nMod vs
                       Cry.TCExp           -> liftEvalType Cry.nExp vs
                       Cry.TCWidth         -> liftEvalType Cry.nWidth vs
                       Cry.TCMin           -> liftEvalType Cry.nMin vs
                       Cry.TCMax           -> liftEvalType Cry.nMax vs
                       Cry.TCCeilDiv       -> liftEvalType Cry.nCeilDiv vs
                       Cry.TCCeilMod       -> liftEvalType Cry.nCeilMod vs
                       Cry.TCLenFromThenTo -> liftEvalType Cry.nLenFromThenTo vs

        Cry.PC {}       -> panic "sizeTypeToNat'" ["PC"]
        Cry.TError {}   -> panic "sizeTypeToNat'" ["TError"]

    Cry.TVar {}         -> pure Nothing
    Cry.TUser _ _ t     -> sizeTypeToNat' t

    Cry.TRec {}         -> panic "sizeTypeToNat'" ["TRec"]
    Cry.TNewtype {}     -> panic "sizeTypeToNat'" ["TNewtype"]



class LiftEvalType a where
  liftEvalType :: a -> [Cry.Nat'] -> Cry.Nat'

instance LiftEvalType (Maybe Cry.Nat') where
  liftEvalType mb args =
    case mb of
      Just a  -> liftEvalType a args
      Nothing -> panic "liftEvalType" ["Malformed numeric type"]

instance LiftEvalType Cry.Nat' where
  liftEvalType v args =
    case args of
      [] -> v
      _  -> panic "liftEvalType" ["Too many arguments"]

instance LiftEvalType b => LiftEvalType (Cry.Nat' -> b) where
  liftEvalType f ts =
    case ts of
      x : xs -> liftEvalType (f x) xs
      []     -> panic "liftEvalType" ["Not enough arguments"]

