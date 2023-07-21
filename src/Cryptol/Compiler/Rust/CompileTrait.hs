module Cryptol.Compiler.Rust.CompileTrait where

import Data.Set(Set)
import Data.Set qualified as Set
import Language.Rust.Syntax qualified as Rust

import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad

lenParamType :: Cry.TParam -> Rust RustType
lenParamType tp =
  do ty <- lookupTParam tp
     -- <ty as cryptol::Lenght>::Length
     let path = simplePath' ["cryptol","Length","Length"]
     pure (Rust.PathTy (Just (Rust.QSelf ty 2)) path ())

-- | Compile a trait.  The set returns type parameters that need
-- an associated Lenght parmaeter.
compileTrait :: Trait -> Rust (Set Cry.TParam, RustWherePredicate)
compileTrait (IRTrait name arg) =
  do tparam <- getTParams
     pure (traitLen, Rust.BoundPredicate [] (tparam arg) [bound] ())
  where
  bound     = Rust.TraitTyParamBound traitName Rust.None ()
  traitName = Rust.PolyTraitRef [] (Rust.TraitRef path) ()

  traitLen  =
    case name of
      PZero     -> Set.singleton arg
      PLiteral  -> Set.singleton arg
      PLogic    -> mempty
      PRing     -> mempty
      PIntegral -> mempty
      PField    -> mempty
      PRound    -> mempty
      PEq       -> mempty
      PCmp      -> mempty
      PSignedCmp-> mempty
      PFLiteral -> mempty

  path = simplePath' pathSegments
  pathSegments =
    [ "cryptol"
    , case name of
        PZero         -> "Zero"
        PLogic        -> "Logic"
        PRing         -> "Ring"
        PIntegral     -> "Integral"
        PField        -> "Field"
        PRound        -> "Round"

        PEq           -> "Eq" -- Reuse Rust's?
        PCmp          -> "Cmp"
        PSignedCmp    -> "SignedCmp"

        PLiteral      -> "Literal"
        PFLiteral     -> "FLiteral"
    ]



