module Cryptol.Compiler.IR.EvalType where

import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Type

evalSizeMin ::
  Eq tname => IRStreamSize tname -> IRStreamSize tname -> IRStreamSize tname
evalSizeMin x y =
  case (x,y) of
    (IRInfSize,_) -> simpSizeType y
    (_,IRInfSize) -> simpSizeType x
    _             -> IRSize (evalSizeType Cry.TCMin [x,y])

class EvalSizeType size where
  simpSizeType  :: Eq tname => size tname -> size tname
  evalSizeType  :: Eq tname => Cry.TFun -> [size tname] -> IRSize tname

instance EvalSizeType IRSize where
  evalSizeType f ts = evalSizeType f (map IRSize ts)
  simpSizeType s =
    case s of
      IRComputedSize f ts -> evalSizeType f ts
      _                   -> s

instance EvalSizeType IRStreamSize where
  simpSizeType ty =
    case ty of
      IRSize s  -> IRSize (simpSizeType s)
      IRInfSize -> ty

  evalSizeType tf args0 =
    case tf of
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
      Cry.TCLenFromThenTo -> liftInfNat dflt args Cry.nLenFromThenTo
    where
    args = map simpSizeType args0
    dflt =
      case (tf, args) of
        (Cry.TCAdd           , [ a, K 0 ])        -> streamSizeToSize a
        (Cry.TCAdd           , [ K 0, b ])        -> streamSizeToSize b

        (Cry.TCSub           , [ a, K 0 ])        -> streamSizeToSize a
        (Cry.TCSub           , [ a, b ]) | a == b -> IRFixedSize 0

        (Cry.TCMul           , [ _, K 0 ])        -> IRFixedSize 0
        (Cry.TCMul           , [ K 0, _ ])        -> IRFixedSize 0
        (Cry.TCMul           , [ IRInfSize, _ ])  -> IRFixedSize 0
        (Cry.TCMul           , [ _, IRInfSize ])  -> IRFixedSize 0
        (Cry.TCMul           , [ a, K 1 ])        -> streamSizeToSize a
        (Cry.TCMul           , [ K 1, b ])        -> streamSizeToSize b

        (Cry.TCDiv           , [ a, K 1 ])        -> streamSizeToSize a

        (Cry.TCMod           , [ _, K 1 ])        -> IRFixedSize 0
        (Cry.TCMod           , [ a, IRInfSize ])  -> streamSizeToSize a

        (Cry.TCExp           , [ _, K 0 ])        -> IRFixedSize 1
        (Cry.TCExp           , [ a, K 1 ])        -> streamSizeToSize a
        (Cry.TCExp           , [ a, IRInfSize ])  -> streamSizeToSize a
        (Cry.TCExp           , [ K 1, _ ])        -> IRFixedSize 1
        (Cry.TCExp           , [ IRInfSize, _])   -> IRFixedSize 1

        (Cry.TCMin           , [ K 0, _ ])        -> IRFixedSize 0
        (Cry.TCMin           , [ _, K 0])         -> IRFixedSize 0
        (Cry.TCMin           , [ IRInfSize, b ])  -> streamSizeToSize b
        (Cry.TCMin           , [ a, IRInfSize ])  -> streamSizeToSize a
        (Cry.TCMin           , [ a, b ]) | a == b -> streamSizeToSize a

        (Cry.TCMax           , [ K 0, b ])        -> streamSizeToSize b
        (Cry.TCMax           , [ a, K 0 ])        -> streamSizeToSize a
        (Cry.TCMax           , [ a, b ]) | a == b -> streamSizeToSize a

        (Cry.TCCeilDiv       , [ a, K 1 ])        -> streamSizeToSize a

        (Cry.TCCeilMod       , [ IRInfSize, _])   -> IRFixedSize 0
        (Cry.TCCeilMod       , [ _, K 1])         -> IRFixedSize 0

        _ -> IRComputedSize tf (map streamSizeToSize args)



-- | Approximate the size of the result when we apply a function
-- to the inputs of the given sizes.
evalSizeTypeSize :: Cry.TFun -> [SizeVarSize] -> SizeVarSize
evalSizeTypeSize tf args =
  case tf of
    Cry.TCAdd           -> LargeSize
    Cry.TCSub           -> op 0
    Cry.TCMul           -> LargeSize
    Cry.TCDiv           -> op 0
    Cry.TCMod           -> op 1
    Cry.TCExp           -> LargeSize
    Cry.TCWidth         -> MemSize
    Cry.TCMin           -> case (op 0, op 1) of
                             (MemSize,_)  -> MemSize
                             (_, MemSize) -> MemSize
                             _            -> LargeSize
    Cry.TCMax           -> case (op 0, op 1) of
                             (MemSize,MemSize) -> MemSize
                             _                 -> LargeSize
    Cry.TCCeilDiv       -> op 0
    Cry.TCCeilMod       -> op 1
    Cry.TCLenFromThenTo -> case (op 0, op 2) of
                             (MemSize,MemSize) -> MemSize
                             _                 -> LargeSize
  where
  op n = case drop n args of
           x : _ -> x
           _     -> panic "evalSizeTypeSize" ["Bad operand"]

-- | Estimate the size of a size-type.
-- Assumes that we already know that the result is finite.
sizeTypeSize :: IRSize tname -> SizeVarSize
sizeTypeSize ty =
  case ty of
    IRFixedSize n
      | n <= maxSizeVal -> MemSize
      | otherwise       -> LargeSize
    IRPolySize x  -> irsSize x
    IRComputedSize f ts -> evalSizeTypeSize f (map sizeTypeSize ts)


class LiftInfNat a where
  liftInfNat :: IRSize tname -> [IRStreamSize tname] -> a -> IRSize tname

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
          Cry.Inf   -> panic "liftInfNat" [ "Unexpected Inf" ]
          Cry.Nat n -> IRFixedSize n
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

--------------------------------------------------------------------------------



