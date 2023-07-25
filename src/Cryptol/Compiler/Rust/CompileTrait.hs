module Cryptol.Compiler.Rust.CompileTrait where

import Data.Set(Set)
import Data.Set qualified as Set
import Language.Rust.Syntax qualified as Rust

import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(unsupported)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.CompileSize
import Cryptol.Compiler.Rust.Monad

traitNeedsLen :: Trait -> Set Cry.TParam
traitNeedsLen (IRTrait name arg)  =
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

lenParamType :: Cry.TParam -> Rust RustType
lenParamType tp =
  do ty <- lookupTParam tp
     -- <ty as cryptol::Lenght>::Length
     let path = simplePath' ["cryptol","Length","Length"]
     pure (Rust.PathTy (Just (Rust.QSelf ty 2)) path ())

-- | Compile a trait.  The set returns type parameters that need
-- an associated Lenght parmaeter.
compileTrait :: Trait -> Rust (Set Cry.TParam, RustWherePredicate)
compileTrait t@(IRTrait name arg) =
  do tparam <- getTParams
     pure (traitNeedsLen t, Rust.BoundPredicate [] (tparam arg) [bound] ())
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

-- | Given a type, compute the parameter for the `Length`
--   trait in the Rust run-time system.
lenParamFor :: Type -> Rust RustExpr
lenParamFor ty =
  case ty of
    TBool           -> pure unitExpr
    TInteger        -> pure unitExpr

    TIntegerMod n
      | Just _ <- isKnownSize n -> pure unitExpr
      | otherwise  -> unsupported "IntdegerMod dynamic size"
                 -- XXX: the representation of the bound needs to be a
                 -- type parameter is it can be either MemSize or LargeSize

    TRational -> pure unitExpr
    TFloat    -> pure unitExpr
    TDouble   -> pure unitExpr
    TWord sz
      | Just _ <- isKnownSize sz -> pure unitExpr
      | otherwise                -> compileSize sz MemSize

    TArray sz elT
      | Just _ <- isKnownSize sz  -> lenParamFor elT
      | otherwise ->
        do vecLen  <- compileSize sz MemSize
           elemLen <- lenParamFor elT
           pure (tupleExpr [vecLen,elemLen])

    TStream {} -> unsupported "lenStream"
    TTuple ts  -> tupleExpr <$> mapM lenParamFor ts
    TFun _ t   -> lenParamFor t
    TPoly tp   -> lookupLenParam tp



lenParamForSize :: Size -> Rust RustExpr
lenParamForSize = undefined
