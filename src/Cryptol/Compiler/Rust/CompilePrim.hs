module Cryptol.Compiler.Rust.CompilePrim where

import Data.Text(Text)
import Data.Text qualified as Text
import qualified Language.Rust.Syntax as Rust
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.Error (panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Utils

data PrimArgs = PrimArgs
  { primTypesOfArgs   :: [Type]         -- ^ Types of arguments (primArgs)
  , primTypeOfResult  :: Type           -- ^ Type of result
  , primTypeArgs      :: [RustType]     -- ^ Compiled type arguemt
  , primLenArgs       :: [RustExpr]     -- ^ Compile length arguments
  , primSizeArgs      :: [RustExpr]     -- ^ Compiled size arguments
  , primArgs          :: [RustExpr]     -- ^ Compiled normal arguments
  }

instance PP PrimArgs where
  pp args =
    vcat
      [ "--- PrimArgs ---------------------------------"
      , parens (commaSep (map pp (primTypesOfArgs args))) <+> "->" <+>
                              pp (primTypeOfResult args)
      , " "
      , "types:"      <+> ppField (primTypeArgs args)
      , "lengths:"    <+> ppField (primLenArgs args)
      , "sizes:"      <+> ppField (primSizeArgs args)
      , "arguments:"  <+> ppField (primArgs args)
      , "---------------------------------------------------"
      ]
    where
    ppField :: RustPP a => [a] -> Doc
    ppField = commaSep . map rustPP

unsupportedPrim :: Doc -> PrimArgs -> Rust a
unsupportedPrim nm args =
  unsupported (vcat [ "primitive" <+> nm, pp args ])


cryPrimArgOwnership :: Cry.PrimIdent -> [Type] -> Type -> [ExprContext]
cryPrimArgOwnership p@(Cry.PrimIdent mo name) argTs _resT
  | mo == Cry.preludeName = prelPrim
  | mo == Cry.floatName   = floatPrim
  | otherwise =
    panic "cryPrimArgOwnership" ["Unknown primitive", show (cryPP p)]
  where
  prelPrim =
    case name of
      "take" -> map ownIfStream argTs

      --- XXX: Others need ownd arguments, especially stream constructors
      _ -> map (const BorrowContext) argTs

  floatPrim = map (const OwnContext) argTs


primArgOwnership :: IRPrim -> [Type] -> Type -> [ExprContext]
primArgOwnership prim argTs resT =
  case prim of
    CryPrim ide -> cryPrimArgOwnership ide argTs resT

    ArrayLit    -> map (const OwnContext) argTs
    ArrayLookup -> [BorrowContext]

    WordLookup  -> [BorrowContext]

    Tuple       -> map (const OwnContext) argTs
    TupleSel {} -> [BorrowContext]

    EqSize      -> [OwnContext,OwnContext]
    LtSize      -> [OwnContext,OwnContext]
    LeqSize     -> [OwnContext,OwnContext]

    Map         -> [OwnContext, OwnContext] -- function is 2nd
    FlatMap     -> [OwnContext, OwnContext] -- function is 2nd
    Zip         -> [OwnContext, OwnContext] -- function is 2nd

    ArrayToStream -> [OwnContext]
    ArrayToWord   -> [OwnContext]
    WordToStream  -> [OwnContext]
    StreamToWord  -> [OwnContext]
    StreamToArray -> [OwnContext]

    Head          -> [OwnContext] -- head needs to own its argument
    Hist          -> []


-- | Emit code for a primitve.
compilePrim :: IRPrim -> PrimArgs -> Rust RustExpr
compilePrim name args =
  case name of
    CryPrim p   -> compileCryptolPrim p args

    ArrayLit    -> pure (callMacro (simplePath "vec") (primArgs args))

    ArrayLookup ->
      case (primArgs args, primSizeArgs args) of
        ([a],[i]) -> pure (indexExpr a i)
        _ -> bad

    WordLookup ->
      size1 \i ->
      arg1  \a ->
        pure (indexExpr a i)

    StreamToArray ->
      arg1 \s -> pure (callMethod s "collect" [])
                -- XXX: type? specify that we want owned?

    ArrayToStream ->
      arg1 \s ->
        pure (callMethod s "into_iter" [])


    Tuple -> pure (tupleExpr (primArgs args))
    TupleSel n _all ->
      arg1 \x -> pure (tupleSelect x n)

    Hist ->
      size1 \i ->
        pure (callMethod (pathExpr (simplePath "this")) "get_history" [i])

    Head ->
      arg1 \x ->
        pure (rustTry (callMethod x "next" []))


    _ -> pure (todoExp (show (pp name))) -- unsupportedPrim (pp name) args
  where
  bad = panic "compilePrim"
          [ "Malformed primitive arguments:"
          , show (pp args)
          ]
  arg1 f =
    case primArgs args of
      [a] -> f a
      _ -> bad

  size1 f =
    case primSizeArgs args of
      [a] -> f a
      _ -> bad




compileCryptolPrim :: Cry.PrimIdent -> PrimArgs -> Rust RustExpr
compileCryptolPrim p@(Cry.PrimIdent mo name) args
  | mo == Cry.preludeName = compileCryptolPreludePrim name args
  | mo == Cry.floatName   = compileCryptolFloatPrim name args
  | otherwise = unsupportedPrim (pp p) args


-- | Primitives defined in `Cryptol.cry`
compileCryptolPreludePrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolPreludePrim name args =
  case name of

    "False" -> pure (litExpr (boolLit False))
    "True"  -> pure (litExpr (boolLit True))

    "#" -> compilePrimAppend args

    -- Literal
    "number" ->
       pure $ mkRustCall (tyTraitMethod "number")
                         (primLenArgs args ++ primSizeArgs args)

    -- Zero
    "zero" ->
       pure $ mkRustCall (tyTraitMethod "zero") (primLenArgs args)


    -- Ring --
    "+"       -> inferMethod "add"
    "-"       -> inferMethod "sub"
    "*"       -> inferMethod "mul"
    "negate"  -> inferMethod "negate"

    -- Sequence --
    "take" ->
      arg1 \x ->
        case getTypeLen (primTypeOfResult args) of
          IRInfSize -> pure x
          IRSize sz ->
            case sz of
              IRFixedSize n -> pure (callMethod x "take"
                                      [ litExpr (mkUSizeLit n) ])
              _ -> size1 \n -> pure (callMethod x "take" [ n ])

{-
    -- Integral --
    "!"       -> undefined
    "@"       -> undefined
-}

    _ -> pure (todoExp (Text.unpack name)) -- unsupportedPrim (pp name) args

{-
    "fromInteger" -> ring "from_integer"
    -- TODO: do we need to figure out if the exponent will fit in
    --       a u32 before calling this? (or call `Integral::to_usize`?)
    "^^" -> Nothing

    "/" -> integral "div"
    "%" -> integral "modulo"
    "toInteger" -> integral "to_integer"

    -- Zero
    "zero" -> Nothing -- TODO

    -- Logic
    "&&" -> logic "and"
    "||" -> logic "or"
    "^" -> logic "xor"
    "complement" -> logic "complement"
    _ -> Nothing
-}
  where
  inferMethod method =
    pure (mkRustCall (typeQualifiedExpr inferType (simplePath method))
                     (primArgs args))

  tyTraitMethod method =
      case primTypeArgs args of
        [ty] -> typeQualifiedExpr ty (simplePath method)
        _    -> panic "tyTraitMethod" ["Expected exactly 1 type argument"]


  bad = panic "compilePreludePrim"
          [ "Malformed primitive arguments:"
          , show (pp args)
          ]

  arg1 f =
    case primArgs args of
      [a] -> f a
      _ -> bad

  size1 f =
    case primSizeArgs args of
      a : _ -> f a
      _ -> bad

  size2 f =
    case primSizeArgs args of
      a : b : _ -> f a b
      _ -> bad




-- | Primitives defined in `Float.cry`
compileCryptolFloatPrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolFloatPrim _ _ = unsupported "floating point primitve" -- XXX




--------------------------------------------------------------------------------

compilePrimAppend :: PrimArgs -> Rust RustExpr
compilePrimAppend args = pure (todoExp "#")
{-
  case primTypesOfArgs args of
    [ TWord (isKnownSize -> Just w1), TWord (isKnownSize -> Just w2) ] ->
      pure (callMacro (simplePath' [cryptolCrate,"append"])
                      (map (litExpr . mkIntLit Rust.Unsuffixed) [ w1, w2 ] ++ primArgs args))

    -- XXX: array + array, array + stream, word + stream

    _ -> unsupportedPrim "#" args
-}
