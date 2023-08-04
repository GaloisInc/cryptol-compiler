module Cryptol.Compiler.IR.Prims where

import Data.Text qualified as Text
import Cryptol.Utils.Ident qualified as Cry
import Cryptol.Compiler.PP

-- | This is for primitivies specifi to the IR
-- (i.e., *not* the primitives of Cryptol)
data IRPrim =
    CryPrim Cry.PrimIdent       -- ^ A Cryptol primitve

  | MakeSeq
    -- ^ Make a sequence literal.
    -- The arguments are the elementes of the sequence
    -- The result type in the call has the type of sequences
    -- we are making.

  | ArrayLookup
    -- ^ (index : usize, xs: Array n T) -> T
    -- The index to look at is in the *size* args of the call
    -- The parameter is the name of the array
    -- Unspecified behavior if index >= n



  | Tuple
    -- ^ Make a tuple.

  | TupleSel Int Int
    -- ^ select_n_of_N(tuple)
    -- Index `n`-th element in a tuple of size `N`

  | EqSize
    -- ^ Check if two sizes are the same.  They are in the size args of the call

  | LeqSize
    -- ^ Compare two size types.  They are in the size args of the call

    -- Iterators
  | Map       -- ^ (xs : Stream n a, f : a -> b) -> Stream n b
  | FlatMap   -- ^ (xs : Stream m a, f : a -> Stream n b) -> Stream (m*n) b
  | Zip       -- ^ (xs : Stream m a, ys : Stream n b) -> Stream (min m n) (a,b)

  | Collect   -- ^  (xs : Stream n a)    -> Array n a
              -- or (xs : Stream n Bool) -> Word n

  | Iter      -- ^   (xs : Array n a)  -> Stream n a
              -- or  (xs : Word n)      -> Stream n Bit


    deriving (Show,Eq,Ord)



--------------------------------------------------------------------------------
-- Pretty printing

instance PP IRPrim where
  pp prim =
    case prim of
      CryPrim i      -> "CRY::" <> cryPP i
      TupleSel n len -> "IR::" <> hcat [ "select_", pp n, "_of_", pp len ]
      _              -> "IR::" <> pp (Text.pack (show prim))


