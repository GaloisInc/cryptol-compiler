module Cryptol.Compiler.IR.Type
  ( module Cryptol.Compiler.IR.Type
  , Cry.TFun(..)
  ) where

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.PP


data IRTrait tname = IRTrait IRTraitName tname

data IRTraitName =
    PZero
  | PLogic
  | PRing
  | PIntegral
  | PField
  | PRound

  | PEq
  | PCmp
  | PSignedCmp

  | PLiteral
  | PFLiteral


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
  | IRPolySize SizeVarSize tname                  -- ^ Polymorphic size; finite
  | IRComputedSize Cry.TFun [IRStreamSize tname]  -- ^ Computed size
    deriving Show

data SizeVarSize =
    MemSize         -- ^ value will fit in usize
  | LargeSize       -- ^ value may be large, use BigInt
    deriving Show

data ParamInfo =
    NumFixed Cry.Nat'
  | NumVar   SizeVarSize
  | TyBool
  | TyNotBool
  | TyAny

newtype FunInstance = FunInstance [ParamInfo]

data IRFunType tname = IRFunType
  { ftTypeParams :: [tname]
  , ftTraits     :: [IRTrait tname]
  , ftSizeParams :: [(tname, IRType tname)]
  , ftParams     :: [IRType tname]
  , ftResult     :: IRType tname
  }
--------------------------------------------------------------------------------
-- Pretty Printing

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

instance PP tname => PP (IRFunType tname) where
  pp ft = fall <+> quall <+> args <+> "->" <+> res
    where
    fall = case ftTypeParams ft of
             [] -> mempty
             as -> braces (commaSep (map pp as))
    quall = case ftTraits ft of
              [] -> mempty
              cs -> parens (commaSep (map pp cs)) <+> "=>"
    args = parens $ commaSep
                  $ map ppNumArg (ftSizeParams ft) ++ map pp (ftParams ft)
    res  = pp (ftResult ft)
    ppNumArg (x,t) = pp x <+> ":" <+> pp t



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
      IRPolySize sz a     ->
        case sz of
          MemSize   -> pp a
          LargeSize -> "!" <> pp a
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


instance (PP tname) => PP (IRTrait tname) where
  pp (IRTrait t x) = pp t <+> pp x

instance PP IRTraitName where
  pp name =
    case name of
      PZero         -> "Zero"
      PLogic        -> "Logic"
      PRing         -> "Ring"
      PIntegral     -> "Integral"
      PField        -> "Field"
      PRound        -> "Round"

      PEq           -> "Eq"
      PCmp          -> "Cmp"
      PSignedCmp    -> "SignedCmp"

      PLiteral      -> "Literal"
      PFLiteral     -> "FLiteral"


