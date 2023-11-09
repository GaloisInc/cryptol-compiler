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

-- | Compute the Rust type used to represent the given Cryptol type.
compileType :: TypeMode -> IR.Type -> Rust RustType
compileType mode ty =
  case ty of
    IR.TBool          -> pure boolType
    IR.TInteger       -> pure (byRef mode cryIntegerType)
    IR.TIntegerMod _  -> pure (byRef mode cryZType)
    IR.TRational      -> pure (byRef mode cryRationalType)
    IR.TFloat         -> pure (simpleType "f32")
    IR.TDouble        -> pure (simpleType "f64")

    IR.TWord sz ->
      pure (byRef mode (maybe cryDWordType cryWordType (IR.isKnownSize sz)))

    IR.TArray _sz t ->
      do elTy <- compileType AsOwned t
         pure
           case mode of
             AsArg   -> refType (sliceType elTy)
             AsOwned -> cryVectorType elTy

    IR.TStream _sz t -> byRef mode . streamOfType <$> compileType AsOwned t

    IR.TTuple ts ->
      case ts of
        [] -> pure unitType
        _  -> do elTs <- traverse (compileType AsOwned) ts
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

    IR.TFun args ret  ->
        funType <$> traverse (compileType AsArg) args
                                 <*> compileType AsOwned ret


byRef :: TypeMode -> RustType -> RustType
byRef mode ownTy =
  case mode of
    AsArg   -> refType ownTy
    AsOwned -> ownTy


-- types
streamOfType :: RustType -> RustType
streamOfType = vectorOfType -- XXX

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

-- XXX
cryZType :: RustType
cryZType = pathType (simplePath' [cryptolCrate,"Z"])

cryRationalType :: RustType
cryRationalType = pathType (simplePath' ["num","BigRational"])

cryArrayType :: Integer -> RustType -> RustType
cryArrayType _n = cryVectorType -- for now we just use vectors for all

cryVectorType :: RustType -> RustType
cryVectorType = vectorOfType

cryDWordType :: RustType
cryDWordType = pathType (simplePath' [cryptolCrate,"DWord"])

cryWordType :: Integer -> RustType
cryWordType bits = Rust.MacTy mac ()
  where
      mac = Rust.Mac (simplePath' [cryptolCrate, "Word"]) tokenStream ()
      tokenStream = Rust.Tree lengthTok
      lengthTok = Rust.Token dummySpan
                    (Rust.LiteralTok (Rust.IntegerTok (show bits)) Nothing)

