module Cryptol.Compiler.IR.Type where

import qualified Cryptol.TypeCheck.TCon as Cry

import Cryptol.Compiler.Monad(panic)
import Cryptol.Compiler.PP


-- | Value types
data IRType tname =
    TBool                                         -- ^ Boolean
  | TWord (IRSize tname)                          -- ^ Bit vector
  | TArray (IRSize tname) (IRType tname)          -- ^ Array
  | TStream (IRStreamSize tname) (IRType tname)   -- ^ Iterators
  | TTuple [IRType tname]                         -- ^ Tuples
  | TPoly tname                                   -- ^ Polymorphic
  deriving Show

data IRStreamSize tname =
    IRInfSize
  | IRSize (IRSize tname)
  deriving Show

-- | Size types
data IRSize tname =
    IRFixedSize Integer                     -- ^ A specific size
  | IRPolySize  tname                       -- ^ Polymorphic size; finite
  | IRComputedSize Cry.TFun [IRStreamSize tname]  -- ^ Computed size
    deriving Show


--------------------------------------------------------------------------------
-- Pretty Printing

instance PP tname => PP (IRType tname) where
  pp ty =
    case ty of
      TBool         -> "Bool"
      TPoly nm      -> pp nm
      TWord sz      -> parensAfter 0 ("Word"  <+> withPrec 1 (pp sz))
      TArray sz t   -> parensAfter 0 ("Array" <+> withPrec 1 (pp sz <+> pp t))
      TStream sz t ->
        parensAfter 0 ("Stream" <+> withPrec 1 (pp sz <+> pp t))
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

