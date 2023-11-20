-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.CompileType where

import qualified Language.Rust.Syntax as Rust

import Cryptol.Compiler.IR.Cryptol qualified as IR
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Names

-- | Different representations for a Cryptol type.
data TypeMode =
    AsArg (Maybe RustLifetime)
    -- ^ Borrowed represewntation, used to pass arguments

  | AsOwned
  -- ^ Owned representation, currently used for locals, function results,
  -- and when the type is a field of another type

-- | Type use
-- XXX: A lot of these complications are because of streams and functions,
-- which should probably be treated specially...
data TypeUse =
    TypeInFunSig  -- ^ Argument or result of a functoin
  | TypeAsParam   -- ^ Type application in a function call
  | TypeAsStored  -- ^ Stored in another type (e.g., array/stream)


-- | Compute the Rust type used to represent the given Cryptol type.
compileType :: TypeUse -> TypeMode -> IR.Type -> Rust RustType
compileType use mode ty =
  case ty of
    IR.TBool          -> pure boolType
    IR.TInteger       -> pure (byRef mode cryIntegerType)
    IR.TIntegerMod _  -> pure (byRef mode cryZType)
    IR.TRational      -> pure (byRef mode cryRationalType)
    IR.TFloat         -> pure (simpleType "f32")
    IR.TDouble        -> pure (simpleType "f64")

    IR.TWord {} ->
      pure
        case mode of
          AsArg r -> cryDWordRefType r
          AsOwned -> cryDWordType

    IR.TArray _sz t ->
      do elTy <- compileType TypeAsStored AsOwned t
         pure
           case mode of
             AsArg a -> refType a (sliceType elTy)
             AsOwned -> cryVectorType elTy

    IR.TStream _sz t ->
      case use of
        TypeInFunSig -> stream <$> compileType TypeAsStored AsOwned t
          where stream el = implTraitType [streamTraitPath el]
        TypeAsParam  -> pure inferType
        TypeAsStored -> unsupported "Streams stored in containers"

    IR.TTuple ts ->
      case ts of
        [] -> pure unitType
        _  -> do elTs <- traverse (compileType use AsOwned) ts
                 pure (byRef mode (tupleType elTs))

    IR.TPoly x ->
      do tyI <- lookupTParam x
         pure
           case mode of
             AsArg a  -> pathType path
                where
                path = Rust.Path False segments ()
                segments = [ Rust.PathSegment tyI   Nothing ()
                           , Rust.PathSegment "Arg" (Just life) ()
                           ]
                life = Rust.AngleBracketed [lifetimeMaybe a] [] [] ()
             AsOwned -> pathType (simplePath tyI)

    IR.TFun args ret  ->
      case use of
        TypeAsStored -> unsupported "function stored in a container"
        TypeAsParam -> pure inferType
        TypeInFunSig ->
          case mode of
            AsOwned -> unsupported "Owned function"
            AsArg r ->
              implFnTraitType
                  <$> traverse (compileType use (AsArg r)) args
                  <*> compileType use AsOwned ret


byRef :: TypeMode -> RustType -> RustType
byRef mode ownTy =
  case mode of
    AsArg r -> refType r ownTy
    AsOwned -> ownTy


streamTraitPath :: RustType -> RustPath
streamTraitPath elT = pathAddTypeSuffix trait [ elT ]
  where
  trait = simplePath' [ cryptolCrate, "Stream" ]

cryIntegerType :: RustType
cryIntegerType = pathType (simplePath' ["num","BigInt"])

cryZType :: RustType
cryZType = pathType (simplePath' [cryptolCrate,"Z"])

cryRationalType :: RustType
cryRationalType = pathType (simplePath' ["num","BigRational"])

cryVectorType :: RustType -> RustType
cryVectorType = vectorOfType

cryDWordType :: RustType
cryDWordType = pathType (simplePath' [cryptolCrate,"DWord"])

cryDWordRefType :: Maybe RustLifetime -> RustType
cryDWordRefType a = pathType (pathAddLifetimeSuffix path [life])
  where
  path = simplePath' [cryptolCrate,"DWordRef"]
  life = case a of
           Just l -> l
           Nothing -> lifetime "_"


