module Cryptol.Compiler.IR.Common where

import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.PP


-- | The size of a numeric type parameter.
data SizeVarSize =
    MemSize         -- ^ value will fit in usize
  | LargeSize       -- ^ value *is* large, use BigInt
    deriving (Eq,Ord,Show)


-- | A pattern in a function instance.
-- NOTE: Ordering of constructors is important.
-- More specific ones should come *before* less specific ones
data ParamInfo =
    NumFixed Cry.Nat'
  | NumVar   SizeVarSize
  | TyBool
  | TyNotBool
  | TyAny
    deriving (Eq,Ord)

-- | A specific instance of a Cryptol function.
newtype FunInstance = FunInstance [ ParamInfo ]

isEmptyInstance :: FunInstance -> Bool
isEmptyInstance (FunInstance ps) = null ps

--------------------------------------------------------------------------------

instance PP FunInstance where
  pp (FunInstance xs) = brackets (commaSep (map pp xs))

instance PP ParamInfo where
  pp info =
    case info of
      NumFixed Cry.Inf     -> "inf"
      NumFixed (Cry.Nat n) -> pp n
      NumVar sz            -> case sz of
                                MemSize   -> "size"
                                LargeSize -> "integer"
      TyBool               -> "bit"
      TyNotBool            -> "!bit"
      TyAny                -> "_"

instance PP SizeVarSize where
  pp sz =
    case sz of
      MemSize   -> "MemSize"
      LargeSize -> "LargeSize"


