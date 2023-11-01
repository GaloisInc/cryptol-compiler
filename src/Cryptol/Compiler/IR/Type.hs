module Cryptol.Compiler.IR.Type
  ( module Cryptol.Compiler.IR.Type
  , SizeVarSize(..)
  , Cry.TFun(..)
  , Cry.Nat'(..)
  , maxSizeVal
  ) where

import Data.Map(Map)
import Data.Map qualified as Map

import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.TypeCheck.TCon qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.PP

import Cryptol.Compiler.IR.Common


-- | A constraint on a type parameter.
data IRTrait tname =
  IRTrait { irTraitName :: IRTraitName, irTraitType :: tname }
  deriving (Functor, Foldable, Traversable)

-- | Names of constraints (aka traits in Rust, classes in Haskell).
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


-- | Value types.
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
    deriving (Eq,Functor,Foldable,Traversable)

-- | Size types that could be infinite.
data IRStreamSize tname =
    IRInfSize                                     -- ^ Infinite size
  | IRSize (IRSize tname)                         -- ^ Finite size
    deriving (Eq,Ord,Functor,Foldable,Traversable)

-- | The name of a size variable.
data IRSizeName tname = IRSizeName { irsName :: tname, irsSize :: SizeVarSize }
  deriving (Eq,Ord,Functor,Foldable,Traversable)

-- | Size "types".  These are typically not erased, to they are really values.
-- These are supposed to be finite.
data IRSize tname =
    IRFixedSize Integer                     -- ^ A specific size
  | IRPolySize (IRSizeName tname)           -- ^ Polymorphic size; finite
  | IRComputedSize Cry.TFun [IRSize tname]  -- ^ Computed size
    deriving (Eq,Ord,Functor,Foldable,Traversable)


-- | The type of a function.
data IRFunType tname = IRFunType
  { ftTypeParams :: [tname]            -- ^ Value type parameters.
  , ftTraits     :: [IRTrait tname]    -- ^ Constraints on the type paramters.
  , ftSizeParams :: [IRSizeName tname] -- ^ Size parameters.
  , ftParams     :: [IRType tname]     -- ^ Normal parameters.
  , ftResult     :: IRType tname       -- ^ Function result.
  } deriving (Functor, Foldable, Traversable)

monoFunType :: [IRType tname] -> IRType tname -> IRFunType tname
monoFunType args res =
  IRFunType
    { ftTypeParams = []
    , ftTraits     = []
    , ftSizeParams = []
    , ftParams     = args
    , ftResult     = res
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
type instance TName ()        = ()


--------------------------------------------------------------------------------
-- Common utilities

pattern K :: Integer -> IRStreamSize tname
pattern K n = IRSize (IRFixedSize n)

-- | Is this a known size type or infinity.
isKnownStreamSize :: IRStreamSize tname  -> Maybe Cry.Nat'
isKnownStreamSize s =
  case s of
    IRInfSize -> Just Cry.Inf
    IRSize si -> Cry.Nat <$> isKnownSize si

-- | Is this a known size type.
isKnownSize :: IRSize tname -> Maybe Integer
isKnownSize sz =
  case sz of
    IRFixedSize n     -> Just n
    IRPolySize {}     -> Nothing
    IRComputedSize {} -> Nothing

-- | Group traits by their variable name.
traitMap :: Ord tname => [IRTrait tname] -> Map tname [IRTraitName]
traitMap ts =
  Map.fromListWith (++) [ (irTraitType t, [irTraitName t]) | t <- ts ]

-- | Get the size part of a finie size.  Panics if infinite.
streamSizeToSize :: IRStreamSize tname -> IRSize tname
streamSizeToSize s =
  case s of
    IRSize x -> x
    IRInfSize -> panic "streamSizeToSize" ["inf"]

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
                     $ parens (commaSep (map pp as)) <+> "=>" <+> pp b


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


