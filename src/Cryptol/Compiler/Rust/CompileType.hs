-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.CompileType where

import qualified Language.Rust.Syntax as Rust

import Cryptol.Compiler.IR.Cryptol qualified as IR
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Names

-- | Different representations for a Cryptol type.
data TypeMode =
    AsArg     -- ^ Borrowed represewntation, used to pass arguments
  | AsOwned
  -- ^ Owned representation, currently used for locals, function results,
  -- and when the type is a field of another type

-- | Type use
data TypeUse =
    TypeInFunSig  -- ^ Argument or result of a functoin
  | TypeAsParam   -- ^ Type application in a function call


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

    IR.TWord sz ->
      pure
        case mode of
          AsArg   -> cryDWordRefType
          AsOwned -> cryDWordType

    IR.TArray _sz t ->
      do elTy <- compileType use AsOwned t
         pure
           case mode of
             AsArg   -> refType (sliceType elTy)
             AsOwned -> cryVectorType elTy

    IR.TStream _sz t ->
      case use of
        TypeInFunSig -> streamOfType <$> compileType use AsOwned t
        TypeAsParam  -> pure inferType

    IR.TTuple ts ->
      case ts of
        [] -> pure unitType
        _  -> do elTs <- traverse (compileType use AsOwned) ts
                 pure (byRef mode (tupleType elTs))

    IR.TPoly x ->
      do tyI <- lookupTParam x
         pure
           case mode of
             -- T::Arg<'_>
             AsArg   -> pathType path
                where
                path = Rust.Path False segments ()
                segments = [ Rust.PathSegment tyI   Nothing ()
                           , Rust.PathSegment "Arg" (Just life) ()
                           ]
                life = Rust.AngleBracketed [Rust.Lifetime "_" ()] [] [] ()
             AsOwned -> pathType (simplePath tyI)

    -- XXX: Are `impls` allowd in these??
    IR.TFun args ret  ->
        funType <$> traverse (compileType use AsArg) args
                                 <*> compileType use AsOwned ret


byRef :: TypeMode -> RustType -> RustType
byRef mode ownTy =
  case mode of
    AsArg   -> refType ownTy
    AsOwned -> ownTy


-- types
streamOfType :: RustType -> RustType
streamOfType elT = implTraitType [ path ]
  where
  path = pathAddTypeSuffix trait [ elT ]
  trait = simplePath' [ cryptolCrate, "Stream" ]

-- XXX
funType :: [RustType] -> RustType -> RustType
funType funArgs funRes =
  pathType $
 simplePath' [ cryptolCrate, "Fun" ] -- (funArgs ++ [funRes]
{-
  where
  
  path = Rust.Path False [ Rust.PathSegment "Fun" (Just params) () ] ()
  params = Rust.AngleBracketed [] (funArgs ++ [funRes]) [] ()
-}

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

cryDWordRefType :: RustType
cryDWordRefType = pathType (pathAddLifetimeSuffix path [lifetime "_"])
  where
  path = simplePath' [cryptolCrate,"DWordRef"]


