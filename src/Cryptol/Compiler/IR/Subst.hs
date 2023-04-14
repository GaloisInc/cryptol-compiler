module Cryptol.Compiler.IR.Subst
  ( IRSubst
  , Subst
  , suEmpty
  , suLookupType
  , suLookupSize
  , suSingleType
  , suSingleSize
  , suAddType
  , suAddSize
  , suMerge
  , suBinds
  , suIsEmpty
  , apSubst
  , ApSubst(..)
  ) where

import Data.Maybe(fromMaybe)
import Data.Map (Map)
import Data.Map qualified as Map

import Cryptol.TypeCheck.Type(TParam)
import Cryptol.Utils.Misc(anyJust,anyJust2)

import Cryptol.Compiler.Monad(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.EvalType

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

suSingleType :: Ord tname => tname -> IRType tname -> IRSubst tname
suSingleType x t = suAddType x t suEmpty

suSingleSize :: Ord tname => tname -> IRStreamSize tname -> IRSubst tname
suSingleSize x t = suAddSize x t suEmpty

suBinds :: Ord tname => tname -> IRSubst tname -> Bool
suBinds x su = x `Map.member` suType su || x `Map.member` suSize su

suIsEmpty :: IRSubst tname -> Bool
suIsEmpty su = Map.null (suType su) && Map.null (suSize su)

-- | Merge two substitutions. Assumes they are disjoint.
suMerge :: Ord tname => IRSubst tname -> IRSubst tname -> IRSubst tname
suMerge su1 su2 =
  IRSubst
    { suType = Map.union (suType su1) (suType su2)
    , suSize = Map.union (suSize su1) (suSize su2)
    }

--------------------------------------------------------------------------------
apSubst :: ApSubst a => IRSubst (TName a) -> a -> a
apSubst su f = fromMaybe f (apSubstMaybe su f)

class Ord (TName a) => ApSubst a where
  apSubstMaybe :: IRSubst (TName a) -> a -> Maybe a

instance ApSubst a => ApSubst [a] where
  apSubstMaybe su = anyJust (apSubstMaybe su)

instance (ApSubst a, ApSubst b, TName a ~ TName b) => ApSubst (a,b) where
  apSubstMaybe su = anyJust2 (apSubstMaybe su) (apSubstMaybe su)

instance Ord tname => ApSubst (IRType tname) where
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
      TArray sz ty    -> uncurry TArray <$> apSubstMaybe su (sz,ty)
      TStream sz ty   -> uncurry TStream <$> apSubstMaybe su (sz,ty)
      TTuple ts       -> TTuple <$> apSubstMaybe su ts
      TPoly x         -> suLookupType x su

instance Ord tname => ApSubst (IRStreamSize tname) where
  apSubstMaybe su size =
    case size of
      IRInfSize {}  -> Nothing
      IRSize s      -> apSubstSizeMaybe su s

apSubstSizeMaybe ::
  Ord tname => IRSubst tname -> IRSize tname -> Maybe (IRStreamSize tname)
apSubstSizeMaybe su size =
  case size of
    IRFixedSize {}        -> Nothing
    IRPolySize x          -> suLookupSize (irsName x) su
    IRComputedSize fun as -> evalSizeType fun <$> apSubstMaybe su as

instance Ord tname => ApSubst (IRSize tname) where
  apSubstMaybe su size =
    do isize <- apSubstSizeMaybe su size
       case isize of
         IRSize s     -> pure s
         IRInfSize {} -> panic "apSubstMaybe@IRSize" ["Unexpecetd IRInfSize"]

instance Ord tname => ApSubst (IRName tname name) where
  apSubstMaybe su (IRName x t) = IRName x <$> apSubstMaybe su t

instance Ord tname => ApSubst (IRFunName tname name) where
  apSubstMaybe su (IRFunName x i t) = IRFunName x i <$> apSubstMaybe su t



instance Ord tname => ApSubst (IRExpr tname name) where
  apSubstMaybe su (IRExpr e) = IRExpr <$> apSubstMaybe su e

instance (Ord tname, ApSubst expr, TName expr ~ tname) =>
   ApSubst (IRExprF tname name expr) where
  apSubstMaybe su expr =
    case expr of
       IRVar nm -> IRVar <$> apSubstMaybe su nm
       IRCall f ts sz es ->
         do let doSz (s,x) = do s' <- apSubstMaybe su s
                                pure (s',x)

            (sz',(f',(ts',es'))) <- anyJust2
                                      (anyJust doSz)
                                      (apSubstMaybe su)
                                      (sz,(f,(ts,es)))
            pure (IRCall f' ts' sz' es')
       IRIf e1 e2 e3 ->
         do (e1',(e2',e3')) <- apSubstMaybe su (e1,(e2,e3))
            pure (IRIf e1' e2' e3')

instance (Ord tname) => ApSubst (IRFunDef tname name) where
  apSubstMaybe su def =
    case def of
      IRFunPrim  -> Nothing
      IRFunDef e -> IRFunDef <$> apSubstMaybe su e

--------------------------------------------------------------------------------

instance PP tname => PP (IRSubst tname) where
  pp su = ppMap (suType su) $$ ppMap (suSize su)
    where ppMap mp = vcat [ pp x <+> ":=" <+> pp y | (x,y) <- Map.toList mp ]
