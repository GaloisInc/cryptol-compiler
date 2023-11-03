-- | Compute value-level free names
module Cryptol.Compiler.IR.Free where

import Data.Set(Set)
import Data.Set qualified as Set

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR

data Free tname name =
  Free
    { freeTop     :: Set (IRFunName name)
    , freeSize    :: Set (IRSizeName tname)
    , freeLocals  :: Set (IRName tname name)
    }

instance (Ord name, Ord tname) => Semigroup (Free tname name) where
  xs <> ys =
    Free
      { freeTop     = Set.union (freeTop xs) (freeTop ys)
      , freeSize    = Set.union (freeSize xs) (freeSize ys)
      , freeLocals  = Set.union (freeLocals xs) (freeLocals ys)
      }

instance (Ord name, Ord tname) => Monoid (Free tname name) where
  mempty =
    Free
      { freeTop     = Set.empty
      , freeSize    = Set.empty
      , freeLocals  = Set.empty
      }

instance (PP tname, PP name) => PP (Free tname name) where
  pp fs =
    vcat
      [ ppSet "freeTop" (freeTop fs)
      , ppSet "freeSize" (freeSize fs)
      , ppSet "freeLocals" (freeLocals fs)
      ]
    where
    ppSet :: PP a => Doc -> Set a -> Doc
    ppSet lab xs = lab <.> ":" <+> hsep (map pp (Set.toList xs))

singleFreeTop :: (Ord tname, Ord name) => IRFunName name -> Free tname name
singleFreeTop x = mempty { freeTop = Set.singleton x }

freeSizes :: (Ord tname, Ord name) => Set (IRSizeName tname) -> Free tname name
freeSizes x = mempty { freeSize = x }

singleFreeLocal :: (Ord tname, Ord name) => IRName tname name -> Free tname name
singleFreeLocal x = mempty { freeLocals = Set.singleton x }

removeLocal ::
  Ord name => IRName tname name -> Free tname name -> Free tname name
removeLocal x xs = xs { freeLocals = Set.delete x (freeLocals xs) }


class (Ord (TName a), Ord (VName a)) => FreeNames a where
  freeNames :: a -> Free (TName a) (VName a)

instance ( FreeNames a, FreeNames b
         , TName a ~ TName b, VName a ~ VName b) =>
  FreeNames (a,b) where
  freeNames (a,b) = freeNames a <> freeNames b

instance (FreeNames a) => FreeNames [a] where
  freeNames = foldr ((<>) . freeNames) mempty

instance (FreeNames expr, Ord name, TName expr ~ tname, VName expr ~ name) =>
  FreeNames (IRCall tname name expr) where
  freeNames c = freeNames (ircFun c, ircArgs c)

instance (FreeNames expr, Ord name, TName expr ~ tname, VName expr ~ name) =>
  FreeNames (IRCallable tname name expr) where
  freeNames call =
    case call of
      IRTopFun c -> freeNames c
      IRFunVal e -> freeNames e

instance (Ord tname, Ord name) => FreeNames (IRTopFunCall tname name) where
  freeNames f = singleFreeTop (irtfName f) <>
                freeSizes (freeSizeNames (map fst (irtfSizeArgs f)))

instance
  (FreeNames expr, TName expr ~ tname, VName expr ~ name, Ord name) =>
  FreeNames (IRExprF tname name expr) where
  freeNames expr =
    case expr of
      IRVar x         -> singleFreeLocal x
      IRCallFun c     -> freeNames c
      IRClosure c     -> freeNames c
      IRLam xs e      -> foldr removeLocal (freeNames e) xs
      IRIf e1 e2 e3   -> freeNames (e1,(e2,e3))
      IRLet x e1 e2   -> freeNames e1 <> removeLocal x (freeNames e2)
      IRStream e      -> freeNames e

instance
  (FreeNames expr, TName expr ~ tname, VName expr ~ name, Ord name) =>
  FreeNames (IRStreamExpr tname name expr) where
  freeNames expr = foldr (removeLocal . fst) fs (irsExterns expr)
    where
    fs = freeNames (map snd (irsExterns expr), (irsInit expr, irsNext expr))

instance (Ord tname, Ord name) => FreeNames (IRExpr tname name) where
  freeNames (IRExpr e) = freeNames e


class Ord (TName a) => FreeSizeNames a where
  freeSizeNames :: a -> Set (IRSizeName (TName a))

instance FreeSizeNames a => FreeSizeNames [a] where
  freeSizeNames = Set.unions . map freeSizeNames

instance Ord tname => FreeSizeNames (IRSize tname) where
  freeSizeNames sz =
    case sz of
      IRFixedSize {}      -> mempty
      IRPolySize x        -> Set.singleton x
      IRComputedSize _ xs -> freeSizeNames xs

instance (Ord tname) => FreeSizeNames (IRStreamSize tname) where
  freeSizeNames sz =
    case sz of
      IRInfSize -> mempty
      IRSize s  -> freeSizeNames s


freeValTypeVars :: Ord tname => IRType tname -> Set tname
freeValTypeVars ty =
  case ty of
    TBool           -> mempty
    TInteger        -> mempty
    TIntegerMod {}  -> mempty
    TRational       -> mempty
    TFloat          -> mempty
    TDouble         -> mempty
    TWord {}        -> mempty
    TArray _ elTy   -> freeValTypeVars elTy
    TStream _ elTy  -> freeValTypeVars elTy
    TTuple ts       -> Set.unions (map freeValTypeVars ts)
    TFun args res   -> Set.unions (map freeValTypeVars (res : args))
    TPoly x         -> Set.singleton x




