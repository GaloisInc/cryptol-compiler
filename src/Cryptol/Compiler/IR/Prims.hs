module Cryptol.Compiler.IR.Prims where

import Data.Text qualified as Text
import Cryptol.Compiler.PP


-- | This is for primitivies specifi to the IR
-- (i.e., *not* the primitives of Cryptol)
data IRPrim = XXX
  deriving (Show,Eq,Ord)



--------------------------------------------------------------------------------
-- Pretty printing

instance PP IRPrim where
  pp = pp . Text.pack . show
