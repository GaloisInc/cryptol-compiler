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
  , suMergeMaybe
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
import Cryptol.Compiler.IR.Type(matchStreamSize)
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

suMergeMaybe ::
  forall tname.
  Ord tname => IRSubst tname -> IRSubst tname -> Maybe (IRSubst tname)
suMergeMaybe su1 su2
  | overlapOk (==)            suType &&
    overlapOk matchStreamSize suSize = Just (suMerge su1 su2)
  | otherwise = Nothing
  where
  overlapOk :: (a -> a -> Bool) -> (IRSubst tname -> Map tname a) -> Bool
  overlapOk eq f = and (Map.intersectionWith eq (f su1) (f su2))

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
      TWord sz        -> TWord <$> apSubstMaybe su sz
      TArray sz ty    -> uncurry TArray <$> apSubstMaybe su (sz,ty)
      TStream sz ty   -> uncurry TStream <$> apSubstMaybe su (sz,ty)
      TTuple ts       -> TTuple <$> apSubstMaybe su ts
      TFun as b       -> uncurry TFun <$> apSubstMaybe su (as,b)
      TPoly x         -> suLookupType x su

instance Ord tname => ApSubst (IRStreamSize tname) where
  apSubstMaybe su size =
    case size of
      IRInfSize {}  -> Nothing
      IRSize s      -> IRSize <$> apSubstMaybe su s

instance Ord tname => ApSubst (IRSize tname) where
  apSubstMaybe su size =
    case size of
      IRFixedSize {}        -> Nothing

      IRPolySize x ->
         do yes <- suLookupSize (irsName x) su
            case yes of
              IRInfSize -> panic "apSubstMaybe@IRSize" ["IRInfSize"]
              IRSize s  -> pure s

      IRComputedSize fun as sz ->
        (\xs -> evalSizeType fun xs sz) <$> apSubstMaybe su as


instance Ord tname => ApSubst (IRName tname name) where
  apSubstMaybe su (IRName x t) = IRName x <$> apSubstMaybe su t

instance Ord tname => ApSubst (IRExpr tname name) where
  apSubstMaybe su (IRExpr e) = IRExpr <$> apSubstMaybe su e

instance (Ord tname, ApSubst expr, TName expr ~ tname) =>
   ApSubst (IRExprF tname name expr) where
  apSubstMaybe su expr =
    case expr of
       IRVar nm -> IRVar <$> apSubstMaybe su nm
       IRCallFun f -> IRCallFun <$> apSubstMaybe su f
       IRClosure f -> IRClosure <$> apSubstMaybe su f
       IRLam xs e  -> uncurry IRLam <$> apSubstMaybe su (xs,e)
       IRIf e1 e2 e3 ->
         do (e1',(e2',e3')) <- apSubstMaybe su (e1,(e2,e3))
            pure (IRIf e1' e2' e3')

       IRLet x e1 e2 ->
        do (x',(e1',e2')) <- apSubstMaybe su (x,(e1,e2))
           pure (IRLet x' e1' e2')
       IRStream s -> IRStream <$> apSubstMaybe su s

instance (Ord tname, ApSubst expr, TName expr ~ tname) =>
  ApSubst (IRStreamExpr tname name expr) where
  apSubstMaybe su expr =
    do (ty, (exts, (re,nex))) <- apSubstMaybe su (irsType expr,
                                                  (irsExterns expr,
                                                  (irsRec expr, irsNext expr)))
       pure IRStreamExpr
              { irsType = ty
              , irsExterns = exts
              , irsRec = re
              , irsNext = nex }

instance ApSubst expr => ApSubst (IRStreamRec expr) where
  apSubstMaybe su re =
    case re of
      NonRecStream -> Nothing
      RecStream e  -> RecStream <$> apSubstMaybe su e

instance (Ord tname) => ApSubst (IRTopFunCall tname name) where
  apSubstMaybe su (IRTopFunCall f ts sz) =
    do let doSz (s,x) = do s' <- apSubstMaybe su s
                           pure (s',x)

       (ts',sz') <- anyJust2 (apSubstMaybe su) (anyJust doSz) (ts,sz)
       pure (IRTopFunCall f ts' sz')

instance
  (Ord tname, ApSubst expr, TName expr ~ tname) =>
                                      ApSubst (IRCallable tname name expr) where
  apSubstMaybe su call =
    case call of
      IRTopFun fu   -> IRTopFun <$> apSubstMaybe su fu
      IRFunVal expr -> IRFunVal <$> apSubstMaybe su expr

instance
  (Ord tname, ApSubst expr, TName expr ~ tname) =>
                                      ApSubst (IRCall tname name expr) where
  apSubstMaybe su (IRCall f ft at t es) =
    do (f',((at',t'),es')) <- apSubstMaybe su (f,((at,t),es))
       -- Note that we don't apply the subtitution to the original type of
       -- the function.
       pure (IRCall f' ft at' t' es')


instance (Ord tname) => ApSubst (IRFunDef tname name) where
  apSubstMaybe su def =
    case def of
      IRFunPrim     -> Nothing
      IRFunDef is e -> IRFunDef is <$> apSubstMaybe su e

--------------------------------------------------------------------------------

instance PP tname => PP (IRSubst tname) where
  pp su = ppMap (suType su) $$ ppMap (suSize su)
    where ppMap mp = vcat [ pp x <+> ":=" <+> pp y | (x,y) <- Map.toList mp ]
