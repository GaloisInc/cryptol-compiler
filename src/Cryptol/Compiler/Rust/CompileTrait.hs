module Cryptol.Compiler.Rust.CompileTrait where

import Language.Rust.Syntax qualified as Rust
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad


compileTrait :: Trait -> Rust RustWherePredicate
compileTrait (IRTrait name arg) =
  do tparam <- getTParams
     pure (Rust.BoundPredicate [] (tparam arg) [bound] ())
  where
  bound     = Rust.TraitTyParamBound traitName Rust.None ()
  traitName = Rust.PolyTraitRef [] (Rust.TraitRef path) ()


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



