module Cryptol.Compiler.IR.Type
  ( module Cryptol.Compiler.IR.Type
  , Cry.TFun(..)
  ) where

import qualified Cryptol.TypeCheck.TCon as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.PP


-- | Value types
data IRType tname =
    TBool                                         -- ^ Boolean
  | TInteger                                      -- ^ Unbounded integer
  | TIntegerMod (IRSize tname)                    -- ^ Z
  | TRational                                     -- ^ Rational number
  | TFloat                                        -- ^ Floating point small
  | TDouble                                       -- ^ Floating point large

  | TSize                                         -- ^ Sizes of arrays, indexing
  | TWord (IRSize tname)                          -- ^ Bit vector
  | TArray (IRSize tname) (IRType tname)          -- ^ Array
  | TStream (IRStreamSize tname) (IRType tname)   -- ^ Iterator
  | TTuple [IRType tname]                         -- ^ Tuple
  | TPoly tname                                   -- ^ Polymorphic
  deriving Show

-- | Size types tha tcould be infinite.
data IRStreamSize tname =
    IRInfSize                                     -- ^ Infinite size
  | IRSize (IRSize tname)                         -- ^ Finite size
  deriving Show

-- | Size types
data IRSize tname =
    IRFixedSize Integer                           -- ^ A specific size
  | IRPolySize  tname                             -- ^ Polymorphic size; finite
  | IRComputedSize Cry.TFun [IRStreamSize tname]  -- ^ Computed size
    deriving Show

data IRFunType tname = IRFunType
  { ftTypeParams :: [tname]
    -- XXX: some constraints on the tnames (e.g., Ring a)
  , ftSizeParams :: [(tname, IRType tname)]
  , ftParams     :: [IRType tname]
  , ftResult     :: IRType tname
  }
--------------------------------------------------------------------------------
-- Pretty Printing

instance PP tname => PP (IRType tname) where
  pp ty =
    case ty of
      TBool         -> "Bool"
      TInteger      -> "Integer"
      TIntegerMod n -> parensAfter 0 ("Z" <+> withPrec 9 (pp n))
      TRational     -> "Rational"
      TFloat        -> "Float"
      TDouble       -> "Double"
      TSize         -> "Size"
      TWord n       -> parensAfter 0 ("Word"  <+> withPrec 9 (pp n))
      TArray n t    -> parensAfter 0 ("Array" <+> withPrec 9 (pp n)
                                              <+> withPrec 1 (pp t))
      TStream n t -> parensAfter 0 ("Stream" <+> withPrec 9 (pp n <+> pp t))
      TPoly nm      -> pp nm
      TTuple ts     -> withPrec 0 (parens (commaSep (map pp ts)))

instance PP tname => PP (IRStreamSize tname) where
  pp ty =
    case ty of
      IRInfSize -> "inf"
      IRSize sz -> pp sz

instance PP tname => PP (IRSize tname) where
  pp ty =
    case ty of
      IRFixedSize size    -> pp size
      IRPolySize a        -> pp a
      IRComputedSize fu ts ->
        case fu of
          Cry.TCAdd            -> ppInfix 1 1 1 "+"
          Cry.TCSub            -> ppInfix 1 1 2 "-"
          Cry.TCMul            -> ppInfix 2 2 2 "*"
          Cry.TCDiv            -> ppInfix 2 2 3 "/"
          Cry.TCMod            -> ppInfix 2 2 3 "%"
          Cry.TCExp            -> ppInfix 3 4 3 "^"
          Cry.TCWidth          -> ppFun "width"
          Cry.TCMin            -> ppFun "min"
          Cry.TCMax            -> ppFun "max"
          Cry.TCCeilDiv        -> ppInfix 2 2 3 "/^"
          Cry.TCCeilMod        -> ppInfix 2 2 3 "%^"
          Cry.TCLenFromThenTo  -> ppFun "lengthFromThenTo"

        where
        ppFun f = parensAfter 8 (f <+> withPrec 9 (hsep (map pp ts)))

        ppInfix n l r op =
          case ts of
            [t1,t2] -> parensAfter n
                         (withPrec l (pp t1) <+> op <+> withPrec r (pp t2))
            _ -> panic "pp@IRSize" ["Malformed infix"]

