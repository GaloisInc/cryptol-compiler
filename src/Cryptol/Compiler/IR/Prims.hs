module Cryptol.Compiler.IR.Prims where

import Data.Text qualified as Text
import Cryptol.Compiler.PP


-- | This is for primitivies specifi to the IR
-- (i.e., *not* the primitives of Cryptol)
data IRPrim =
    MakeSeq
    -- ^ Make a sequence literal
    -- The arguments are the elementes of the sequence
    -- The result type in the call has the type of sequences
    -- we are making.

  | TupleSel Int Int
    -- ^ select_n_of_N(tuple)
    -- Index `n`-th element in a tuple of size `N`

    -- Iterators
  | Map       -- ^ (xs : Stream n a, f : a -> b) -> Stream n b
  | FlatMap   -- ^ (xs : Stream m a, f : a -> Stream n b) -> Stream (m*n) b
  | Zip       -- ^ (xs : Stream m a, ys : Stream n b) -> Stream (min m n) (a,b)

  | Collect   -- ^  (xs : Stream n a)    -> Array n a
              -- or (xs : Stream n Bool) -> Word n

  | Iter      -- ^   (xs : Array n a)  -> Stream n a
              -- or  (xs : Wor n)      -> Stream n Bit

    deriving (Show,Eq,Ord)



--------------------------------------------------------------------------------
-- Pretty printing

instance PP IRPrim where
  pp prim =
    case prim of
      TupleSel n len -> hcat [ "select_", pp n, "_of_", pp len ]
      _              -> pp (Text.pack (show prim))
