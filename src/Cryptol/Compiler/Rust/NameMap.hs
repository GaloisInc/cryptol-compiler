-- | Maps names to rust identifier, and avoids reusing identifier.
module Cryptol.Compiler.Rust.NameMap where

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
addName :: (Ord a, RustIdent a) => a -> NameMap a -> (Rust.Ident, NameMap a)
addName x mp =
  (i, mp { lUsed = Set.insert i (lUsed mp), lMap  = Map.insert x i (lMap mp) })
  where
  used = lUsed mp
  i    = rustIdentAvoiding used (rustIdent x)


-- | Lookup a name.  Panics if not defined.
lookupName :: (PP a, Ord a) => a -> NameMap a -> Rust.Ident
lookupName x mp =
  case Map.lookup x (lMap mp) of
    Just a  -> a
    Nothing -> panic "lookupName"
                 [ "Undefined name"
                 , show (pp x)
                 ]


