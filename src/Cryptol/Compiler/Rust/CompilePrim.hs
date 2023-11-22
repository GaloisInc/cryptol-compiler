module Cryptol.Compiler.Rust.CompilePrim where

import Data.Text(Text)
import Data.Text qualified as Text
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.Utils.Ident qualified as Cry

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Compiler.Error (panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.CompileSize
import Cryptol.Compiler.Rust.CompileType
import Cryptol.Compiler.Rust.Utils

data PrimArgs = PrimArgs
  { primInstance      :: FunInstance
  , primTypesOfArgs   :: [Type]         -- ^ Types of arguments (primArgs)
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


cryPrimArgOwnership ::
  Cry.PrimIdent -> Int -> [Type] -> Type -> ([ExprContext], [ExprContext])
cryPrimArgOwnership p@(Cry.PrimIdent mo name) szArgs argTs _resT
  | mo == Cry.preludeName = prelPrim
  | mo == Cry.floatName   = floatPrim
  | otherwise =
    panic "cryPrimArgOwnership" ["Unknown primitive", show (cryPP p)]
  where
  prelPrim =
    case name of
      "take"   -> (replicate szArgs OwnContext, map ownIfStream argTs)
      "fromTo" -> (replicate szArgs OwnContext, [])
      "zip"    -> (replicate szArgs OwnContext, [OwnContext,OwnContext])
      "map"    -> (replicate szArgs OwnContext, [OwnContext,OwnContext])

      -- Logic
      "&&"         -> dflt
      "||"         -> dflt
      "^"          -> dflt
      "complement" -> dflt

      -- Comparisons
      "==" -> dflt
      "!=" -> dflt
      "<"  -> dflt
      ">"  -> dflt
      "<=" -> dflt
      ">=" -> dflt

      -- Ring --
      "+"       -> dflt
      "-"       -> dflt
      "*"       -> dflt
      "negate"  -> dflt

      --- XXX: Others need ownd arguments, especially stream constructors
      _ -> dflt

  floatPrim = dflt

  dflt = (replicate szArgs BorrowContext, map (const BorrowContext) argTs)

primArgOwnership :: IRPrim -> Int -> [Type] -> Type -> ([ExprContext], [ExprContext])
primArgOwnership prim szArgs argTs resT =
  case prim of
    CryPrim ide   -> cryPrimArgOwnership ide szArgs argTs resT

    ArrayLit      -> ([], map (const OwnContext) argTs)
    ArrayLookup   -> ([OwnContext], [BorrowContext])

    WordLookup    -> ([OwnContext], [BorrowContext])

    Tuple         -> ([], map (const OwnContext) argTs)
    TupleSel {}   -> ([], [BorrowContext])

    EqSize        -> ([BorrowContext,BorrowContext], [])
    LtSize        -> ([BorrowContext,BorrowContext], [])
    LeqSize       -> ([BorrowContext,BorrowContext], [])

    Map           -> ([], [OwnContext, OwnContext]) -- function is 2nd
    FlatMap       -> ([], [OwnContext, OwnContext]) -- function is 2nd
    Zip           -> ([], [OwnContext, OwnContext]) -- function is 2nd

    ArrayToStream -> ([], [OwnContext])
    ArrayToWord   -> ([], [OwnContext])
    WordToStream  -> ([], [OwnContext])
    StreamToWord  -> ([], [OwnContext])
    StreamToArray -> ([], [OwnContext])

    Head          -> ([], [OwnContext]) -- head needs to own its argument
    Hist          -> ([OwnContext], [])


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
      arg1 \x -> pure (callMethod ((callMethod (tupleSelect x n) "as_arg" []))
                                  "clone_arg" [])

    Hist ->
      size1 \i ->
        pure (callMethod (pathExpr (simplePath "this")) "get_history" [i])

    Head ->
      arg1 \x ->
        pure (rustTry (callMethod x "next" []))

    Map ->
      arg2 \xs f ->
        pure (mkRustCall (pathExpr (rtsName "cry_map")) [f,xs])

    Zip ->
      arg2 \xs ys ->
        pure (callMethod xs "zip" [ys])


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

  arg2 f =
    case primArgs args of
      [a,b] -> f a b
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

    "fromTo" ->
      do (from,fromSz) <- getSizeArg OwnContext args 0
         (to,toSz)     <- getSizeArg OwnContext args 1
         resT <- case primTypeOfResult args of
                   TStream _ el -> compileType TypeInFunSig AsOwned el
                   _ -> panic "fromTo" ["Expected a stream result"]
         let fu = case toSz of
                    MemSize   -> rtsName "from_to_usize"
                    LargeSize -> rtsName "from_to_uint"
             fuE = pathExpr (pathAddTypeSuffix fu [resT])

         let from' = if fromSz == toSz
                      then from
                      else callMethod from "into" []
         pure (mkRustCall fuE (primLenArgs args ++ [from',to]))

{-
    -- Integral --
    "!"       -> undefined
    "@"       -> undefined
-}


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
-}
    -- Logic
    "&&"         -> inferMethod "and"
    "||"         -> inferMethod "or"
    "^"          -> inferMethod "xor"
    "complement" -> inferMethod "complement"

    -- Comparisons
    "==" -> arg2 (\x y -> pure (binExpr Rust.EqOp x y))
    "!=" -> arg2 (\x y -> pure (binExpr Rust.NeOp x y))
    "<"  -> arg2 (\x y -> pure (binExpr Rust.LtOp x y))
    ">"  -> arg2 (\x y -> pure (binExpr Rust.GtOp x y))
    "<=" -> arg2 (\x y -> pure (binExpr Rust.LeOp x y))
    ">=" -> arg2 (\x y -> pure (binExpr Rust.GeOp x y))

    _ -> pure (todoExp (Text.unpack name)) -- unsupportedPrim (pp name) args
  where
  -- XXX: special case if on streams
  inferMethod method =
    do ty <- compileType TypeInFunSig AsOwned (primTypeOfResult args)
       pure (mkRustCall (typeQualifiedExpr ty (simplePath method))
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

  arg2 f =
    case primArgs args of
      [a,b] -> f a b
      _ -> bad



  size1 f =
    case primSizeArgs args of
      a : _ -> f a
      _ -> bad



-- Get the n-th size argument.  Note that `n` here is in the Cryptol
-- scheme, not the actual parameter for this particular instance.
-- Returns `Nothing` if the size is `inf`
getStreamSizeArg ::
  ExprContext -> PrimArgs -> Int -> Rust (Maybe (RustExpr, SizeVarSize))
getStreamSizeArg ctxt args = go ips (primSizeArgs args)
  where
  FunInstance ips = primInstance args
  go inst sizeParams n =
    case inst of
      [] -> panic "getStreamSizeArg" ["Missing arg"]
      p : more ->
       case p of
         TyBool    -> go more sizeParams n
         TyNotBool -> go more sizeParams n
         TyAny     -> go more sizeParams n

         NumVar sz ->
           case sizeParams of
             x : rest
               | n > 0 -> go more rest (n-1)
               | otherwise -> pure (Just (x,sz))

             _ -> panic "getStreamSizeArg" ["Missing size argument"]

         _ | n > 0 -> go more sizeParams (n-1)

         NumFixed fi ->
           case fi of
             Cry.Inf   -> pure Nothing
             Cry.Nat nu ->
               do let sz = if nu > maxSizeVal then LargeSize else MemSize
                  e <- compileSize ctxt (IRFixedSize nu) sz
                  pure (Just (e,sz))

getSizeArg :: ExprContext -> PrimArgs -> Int -> Rust (RustExpr, SizeVarSize)
getSizeArg ctxt args n =
  do mb <- getStreamSizeArg ctxt args n
     case mb of
       Just a  -> pure a
       Nothing -> panic "getizeArg" ["Inf"]


rtsName :: Rust.Ident -> RustPath
rtsName x = simplePath' [ cryptolCrate, x ]




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
