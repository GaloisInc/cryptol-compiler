-- | Maps names to rust identifier, and avoids reusing identifier.
module Cryptol.Compiler.Rust.NameMap where

import Data.Text qualified as Text
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set

import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.Rust.Names

-- | Associate names with Rust identifiers, and keeps track of which
-- rust identifiers we've already used.
data NameMap a = NameMap
  { lUsed :: Set Rust.Ident
  , lMap  :: Map a Rust.Ident
  }

-- | An empty mpa.
emptyNameMap :: Ord a => NameMap a
emptyNameMap = NameMap { lUsed = mempty, lMap  = mempty }

-- | Pick a Rust name for something, ensuring that it does not clash with
-- any previously used names.
addName :: (Ord a, RustIdent a) =>
  (String -> String) -> a -> NameMap a -> (Rust.Ident, NameMap a)
addName norm x mp =
  (i, mp { lUsed = Set.insert i (lUsed mp), lMap  = Map.insert x i (lMap mp) })
  where
  used = lUsed mp
  i    = rustIdentAvoiding norm used (rustIdent x)

removeName :: (Ord a) => a -> Rust.Ident -> NameMap a -> NameMap a
removeName x i nm = nm { lUsed = Set.delete i (lUsed nm)
                       , lMap  = Map.delete x (lMap nm)
                       }

-- | Lookup a name.  Panics if not defined.
lookupName :: (PP a, Ord a) => a -> NameMap a -> Rust.Ident
lookupName x mp =
  case Map.lookup x (lMap mp) of
    Just a  -> a
    Nothing -> panic "lookupName"
                 [ "Undefined name"
                 , show (pp x)
                 , "Available names"
                 , show (pp mp)
                 ]

instance PP a => PP (NameMap a) where
  pp nm = vcat [ pp x <+> "->" <+> pp (Text.pack (show i))
               | (x,i) <- Map.toList (lMap nm) ]


