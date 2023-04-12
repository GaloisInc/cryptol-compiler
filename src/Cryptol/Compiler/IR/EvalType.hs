module Cryptol.Compiler.IR.EvalType where

import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.Monad(panic)
import Cryptol.Compiler.IR.Type

-- | Largest value that will fit in a "size".
maxSizeVal :: Integer
maxSizeVal = 2^(64::Int) - 1

evalSizeType :: Cry.TFun -> [IRStreamSize tname] -> IRStreamSize tname
evalSizeType tf args =
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
  where dflt = IRSize (IRComputedSize tf args)

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
    Cry.TCWidth         -> op 0
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
    IRPolySize sz _     -> sz
    IRComputedSize f ts -> evalSizeTypeSize f (map infSizeTypeSize ts)
      where
      infSizeTypeSize x =
        case x of
          IRInfSize -> LargeSize
          IRSize s  -> sizeTypeSize s



class LiftInfNat a where
  liftInfNat ::
    IRStreamSize tname -> [IRStreamSize tname] -> a -> IRStreamSize tname

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


