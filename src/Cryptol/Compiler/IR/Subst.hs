module Cryptol.Compiler.IR.Subst
  ( Subst
  , suEmpty
  , suLookupType
  , suLookupSize
  , suAddType
  , suAddSize
  , suBinds
  , suIsEmpty
  , apSubst
  , ApSubst(..)
  , evalSizeType
  ) where

import Data.Maybe(fromMaybe)
import Data.Map (Map)
import Data.Map qualified as Map

import Cryptol.TypeCheck.Type(TParam)
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.Utils.Misc(anyJust,anyJust2)

import Cryptol.Compiler.Monad(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Type

type Subst = IRSubst TParam

data IRSubst tname = IRSubst
  { suType  :: Map tname (IRType tname)
  , suSize  :: Map tname (IRStreamSize tname)
  }

suEmpty :: IRSubst tname
suEmpty = IRSubst { suType = Map.empty, suSize = Map.empty }

suLookupType :: Ord tname => tname -> IRSubst tname -> Maybe (IRType tname)
suLookupType x = Map.lookup x . suType

suLookupSize ::
  Ord tname => tname -> IRSubst tname -> Maybe (IRStreamSize tname)
suLookupSize x = Map.lookup x . suSize

suAddType ::
  Ord tname => tname -> IRType tname -> IRSubst tname -> IRSubst tname
suAddType x t su = su { suType = Map.insert x t (suType su) }

suAddSize ::
  Ord tname => tname -> IRStreamSize tname -> IRSubst tname -> IRSubst tname
suAddSize x t su = su { suSize = Map.insert x t (suSize su) }

suBinds :: Ord tname => tname -> IRSubst tname -> Bool
suBinds x su = x `Map.member` suType su || x `Map.member` suSize su

suIsEmpty :: IRSubst tname -> Bool
suIsEmpty su = Map.null (suType su) && Map.null (suSize su)


--------------------------------------------------------------------------------

evalSizeType :: Cry.TFun -> [IRStreamSize tname] -> IRStreamSize tname
evalSizeType tf args =
  case tf of
    Cry.TCAdd           -> liftInfNat dflt args Cry.nAdd
    Cry.TCSub           -> liftInfNat dflt args Cry.nSub
    Cry.TCMul           -> liftInfNat dflt args Cry.nMul
    Cry.TCDiv           -> liftInfNat dflt args Cry.nDiv
    Cry.TCMod           -> liftInfNat dflt args Cry.nMod
    Cry.TCExp           -> liftInfNat dflt args Cry.nExp
    Cry.TCWidth         -> liftInfNat dflt args Cry.nWidth
    Cry.TCMin           -> liftInfNat dflt args Cry.nMin
    Cry.TCMax           -> liftInfNat dflt args Cry.nMax
    Cry.TCCeilDiv       -> liftInfNat dflt args Cry.nCeilDiv
    Cry.TCCeilMod       -> liftInfNat dflt args Cry.nCeilMod
    Cry.TCLenFromThenTo -> liftInfNat dflt args Cry.nLenFromThenTo
  where dflt = IRSize (IRComputedSize tf args)



class LiftInfNat a where
  liftInfNat ::
    IRStreamSize tname -> [IRStreamSize tname] -> a -> IRStreamSize tname

instance LiftInfNat a => LiftInfNat (Maybe a) where
  liftInfNat dflt xs a =
    case a of
      Just v  -> liftInfNat dflt xs v
      Nothing -> panic "liftInfNat" ["Malformed size type: partial"]

instance LiftInfNat Cry.Nat' where
  liftInfNat _ xs a =
    case xs of
      [] ->
        case a of
          Cry.Inf   -> IRInfSize
          Cry.Nat n -> IRSize (IRFixedSize n)
      _  -> panic "liftInfNat" ["Malformed size type: extra arguments"]

instance LiftInfNat a => LiftInfNat (Cry.Nat' -> a) where
  liftInfNat dflt xs f =
    case xs of
      y : ys ->
        case y of
          IRInfSize              -> liftInfNat dflt ys (f Cry.Inf)
          IRSize (IRFixedSize i) -> liftInfNat dflt ys (f (Cry.Nat i))
          _                      -> dflt

      [] -> panic "liftInfNat" ["Malformed size type: missing arguments"]

--------------------------------------------------------------------------------
apSubst :: ApSubst a => IRSubst (TName a) -> a -> a
apSubst su f = fromMaybe f (apSubstMaybe su f)

class Ord (TName a) => ApSubst a where
  type TName a
  apSubstMaybe :: IRSubst (TName a) -> a -> Maybe a

instance ApSubst a => ApSubst [a] where
  type TName [a] = TName a
  apSubstMaybe su = anyJust (apSubstMaybe su)

instance Ord tname => ApSubst (IRType tname) where
  type TName (IRType tname) = tname
  apSubstMaybe su irType =
    case irType of
      TBool           -> Nothing
      TInteger        -> Nothing
      TIntegerMod sz  -> TIntegerMod <$> apSubstMaybe su sz
      TRational       -> Nothing
      TFloat          -> Nothing
      TDouble         -> Nothing
      TSize           -> Nothing
      TWord sz        -> TWord <$> apSubstMaybe su sz
      TArray sz ty    -> uncurry TArray <$>
                           anyJust2 (apSubstMaybe su) (apSubstMaybe su) (sz,ty)
      TStream sz ty   -> uncurry TStream <$>
                           anyJust2 (apSubstMaybe su) (apSubstMaybe su) (sz,ty)
      TTuple ts       -> TTuple <$> anyJust (apSubstMaybe su) ts
      TPoly x         -> suLookupType x su

instance Ord tname => ApSubst (IRStreamSize tname) where
  type TName (IRStreamSize tname) = tname
  apSubstMaybe su size =
    case size of
      IRInfSize {}  -> Nothing
      IRSize s      -> apSubstSizeMaybe su s

apSubstSizeMaybe ::
  Ord tname => IRSubst tname -> IRSize tname -> Maybe (IRStreamSize tname)
apSubstSizeMaybe su size =
  case size of
    IRFixedSize {} -> Nothing
    IRPolySize x   -> suLookupSize x su
    IRComputedSize fun as -> evalSizeType fun <$> apSubstMaybe su as

instance Ord tname => ApSubst (IRSize tname) where
  type TName (IRSize tname) = tname
  apSubstMaybe su size =
    do isize <- apSubstSizeMaybe su size
       case isize of
         IRSize s     -> pure s
         IRInfSize {} -> panic "apSubstMaybe@IRSize" ["Unexpecetd IRInfSize"]

--------------------------------------------------------------------------------

instance PP tname => PP (IRSubst tname) where
  pp su = ppMap (suType su) $$ ppMap (suSize su)
    where ppMap mp = vcat [ pp x <+> ":=" <+> pp y | (x,y) <- Map.toList mp ]
