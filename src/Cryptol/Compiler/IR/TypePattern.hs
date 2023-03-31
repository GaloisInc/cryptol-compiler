module Cryptol.Compiler.IR.TypePattern where

import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.Monad(panic)
import Cryptol.Compiler.IR.Type


data IRTypePattern =
    ExactlyBool
  | AnyType
    deriving (Eq,Ord)
    -- The ord instance should have more specific patterns first

data IRStreamSizePattern =
    ExactNumType Cry.Nat'   -- ^ A specific number
  | SizeFinNumType          -- ^ fin n, n fits in size_t
  | LargeFinNumType         -- ^ fin
  | AnySizeType             -- ^ No restrictions
    deriving (Eq,Ord)
    -- The ord instance should have more specific patterns first


{-
-- matchIRStreamSizePattern :: IRStreamSizePattern -> IRStreamSize tname ->
matchIRStreamSizePattern pat ty =
  case pat of
    ExactNumType k ->
      case ty of
        IRInfSize ->
        IRSize sz ->
          case sz of
            IRFixedSize k1 ->
            _
-}


--------------------------------------------------------------------------------

data IRInstanceMap val =
    SizeParam [(IRStreamSizePattern, IRInstanceMap val)]
  | TypeParam [(IRTypePattern, IRInstanceMap val)]   -- ^ More specific first
  | InstanceType val


{- | Create an instance with the given patterns.
Note that there should be a pattern for each type parameter,
use the `AnyType` or `AnySizeType` if there are no constraints on
the parameter. -}
newInstanceMap ::
  [ Either IRTypePattern IRStreamSizePattern ] -> a -> IRInstanceMap a
newInstanceMap params def =
  case params of
    [] -> InstanceType def
    Left typ : more -> TypeParam [ (typ, newInstanceMap more def) ]
    Right sp : more -> SizeParam [ (sp,  newInstanceMap more def) ]

unionInstanceMap :: IRInstanceMap a -> IRInstanceMap a -> IRInstanceMap a
unionInstanceMap i1 i2 =
  case (i1,i2) of
    (SizeParam p1, SizeParam p2) -> SizeParam (mergePats p1 p2)
    (TypeParam p1, TypeParam p2) -> TypeParam (mergePats p1 p2)
    _ -> panic "(<>)@IRInstance" [ "Malformed instance merge." ]

mergePats ::
  Ord pat =>
  [(pat, IRInstanceMap a)] ->
  [(pat, IRInstanceMap a)] ->
  [(pat, IRInstanceMap a)]
mergePats xs ys =
  case (xs,ys) of
    ((xp,k1) : more_xs, (yp,k2) : more_ys) ->
       case compare xp yp of
         LT -> (xp, k1)                     : mergePats more_xs ys
         EQ -> (xp, unionInstanceMap k1 k2) : mergePats more_xs more_ys
         GT -> (yp, k2)                     : mergePats xs      more_ys
    ([],[]) -> []
    _ -> panic "mergeTypePats" ["Mismatched lengths"]

--------------------------------------------------------------------------------


