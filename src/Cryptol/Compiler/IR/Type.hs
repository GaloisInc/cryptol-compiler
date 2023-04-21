module Cryptol.Compiler.IR.Type
  ( module Cryptol.Compiler.IR.Type
  , SizeVarSize(..)
  , Cry.TFun(..)
  ) where

import Cryptol.TypeCheck.TCon qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.PP

import Cryptol.Compiler.IR.Common


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
    deriving (Eq,Ord)


-- | Value types
data IRType tname =
    TBool                                         -- ^ Boolean
  | TInteger                                      -- ^ Unbounded integer
  | TIntegerMod (IRSize tname)                    -- ^ Z
  | TRational                                     -- ^ Rational number
  | TFloat                                        -- ^ Floating point small
  | TDouble                                       -- ^ Floating point large
  | TWord (IRSize tname)                          -- ^ Bit vector

  | TArray (IRSize tname) (IRType tname)          -- ^ Array
  | TStream (IRStreamSize tname) (IRType tname)   -- ^ Iterator
  | TTuple [IRType tname]                         -- ^ Tuple

  | TFun [IRType tname] (IRType tname)            -- ^ Function types

  | TPoly tname                                   -- ^ Polymorphic
    deriving Eq

-- | Size types tha tcould be infinite.
data IRStreamSize tname =
    IRInfSize                                     -- ^ Infinite size
  | IRSize (IRSize tname)                         -- ^ Finite size
    deriving Eq

-- | The name of a size variable
data IRSizeName tname = IRSizeName { irsName :: tname, irsSize :: SizeVarSize }
  deriving (Eq,Ord)

-- | Size types
data IRSize tname =
    IRFixedSize Integer                           -- ^ A specific size
  | IRPolySize (IRSizeName tname)                 -- ^ Polymorphic size; finite
  | IRComputedSize Cry.TFun [IRStreamSize tname]  -- ^ Computed size
    deriving Eq


-- | The type of a function declaration
data IRFunType tname = IRFunType
  { ftTypeParams :: [tname]
  , ftTraits     :: [IRTrait tname]
  , ftSizeParams :: [IRSizeName tname]
  , ftParams     :: [IRType tname]
  , ftResult     :: IRType tname
  }


--------------------------------------------------------------------------------
-- Access to the tname parameter

type family TName t

type instance TName (IRType tname)       = tname
type instance TName (IRSizeName tname)   = tname
type instance TName (IRSize tname)       = tname
type instance TName (IRStreamSize tname) = tname
type instance TName (IRFunType tname)    = tname

-- These are a bit hacky, but are convenient for when the actual
-- things are stored in one of these containers
type instance TName [a]       = TName a
type instance TName (Maybe a) = TName a
type instance TName (a,b)     = TName b


--------------------------------------------------------------------------------
-- Pretty Printing

instance PP tname => PP (IRFunType tname) where
  pp ft = fall <+> quall <+> args <+> "->" <+> res
    where
    fall = case ftTypeParams ft of
             [] -> mempty
             as -> braces (commaSep (map pp as))
    quall = case ftTraits ft of
              [] -> mempty
              cs -> parens (commaSep (map pp cs)) <+> "=>"
    args = withTypes
         $ parens $ commaSep
         $ map pp (ftSizeParams ft) ++ map pp (ftParams ft)
    res  = pp (ftResult ft)



instance PP tname => PP (IRType tname) where
  pp ty =
    case ty of
      TBool         -> "Bool"
      TInteger      -> "Integer"
      TIntegerMod n -> parensAfter 0 ("Z" <+> withPrec 9 (pp n))
      TRational     -> "Rational"
      TFloat        -> "Float"
      TDouble       -> "Double"
      TWord n       -> parensAfter 0 ("Word"  <+> withPrec 9 (pp n))
      TArray n t    -> parensAfter 0 ("Array" <+> withPrec 9 (pp n)
                                              <+> withPrec 1 (pp t))
      TStream n t -> parensAfter 0 ("Stream" <+> withPrec 9 (pp n <+> pp t))
      TPoly nm      -> pp nm
      TTuple ts     -> withPrec 0 (parens (commaSep (map pp ts)))

      TFun as b     -> parensAfter 0
                     $ withPrec 0
                     $ parens (commaSep (map pp as)) <+> "->" <+> pp b


instance (PP tname) => PP (IRSizeName tname) where
  pp (IRSizeName x t) =
    getPPCfg \cfg ->
      let nm = case t of
                 MemSize   -> pp x
                 LargeSize -> "!" <> pp x
      in if ppShowTypes cfg
              then parensAfter 0 (nm <+> ":" <+> pp t)
              else nm

instance PP tname => PP (IRStreamSize tname) where
  pp ty =
    case ty of
      IRInfSize -> "inf"
      IRSize sz -> pp sz

instance PP tname => PP (IRSize tname) where
  pp ty =
    case ty of
      IRFixedSize size -> pp size
      IRPolySize x -> pp x
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
  pp (IRTrait t x) = pp x <.> ":" <+> pp t

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


