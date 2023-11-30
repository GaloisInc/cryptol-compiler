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
import Cryptol.Compiler.Rust.CompileTrait
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
cryPrimArgOwnership p@(Cry.PrimIdent mo name) szArgs argTs resT
  | mo == Cry.preludeName = prelPrim
  | mo == Cry.floatName   = floatPrim
  | otherwise =
    panic "cryPrimArgOwnership" ["Unknown primitive", show (cryPP p)]
  where
  prelPrim =
    case name of
      "take"        -> (ownSizes, map ownIfStream argTs)
      "drop"        -> (ownSizes, map ownIfStream argTs)
      "fromTo"      -> (ownSizes, [])
      "infFrom"     -> ([], [BorrowContext])
      "infFromThen" -> ([], [BorrowContext,BorrowContext])
      "zip"         -> (ownSizes, [OwnContext,OwnContext])
      "map"         -> (ownSizes, [OwnContext,OwnContext])
      "join"        -> (ownSizes, [OwnContext])
      "split"       -> (ownSizes, [OwnContext])
      "#"           -> (ownSizes
                       , case resT of
                           TWord {} -> [BorrowContext,BorrowContext]
                           _        -> [OwnContext,OwnContext]
                       )
      "transpose"   -> (ownSizes, [BorrowContext])
      "reverse"     -> (ownSizes, [OwnContext])
      "!"           -> (ownSizes, map ownIfStream argTs)
      "@"           -> (ownSizes, map ownIfStream argTs)

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

      -- Polynomial Arithmetic
      "pmult" -> (replicate szArgs OwnContext, [BorrowContext,BorrowContext])
      "pmod"  -> (replicate szArgs OwnContext, [BorrowContext,BorrowContext])


      -- Ring --
      "+"       -> dflt
      "-"       -> dflt
      "*"       -> dflt
      "negate"  -> dflt
      "^^"      -> dflt

      -- Shifts --
      --XXX

      --- XXX: Others need ownd arguments, especially stream constructors
      _ -> dflt

  floatPrim = dflt

  ownSizes = replicate szArgs OwnContext
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
        ([a],[i]) -> pure (callMethod (indexExpr a i) "clone" [])
        _ -> bad

    WordLookup ->
      size1 \i ->
      arg1  \a ->
        pure (callMethod a "index_msb" [i])

    StreamToArray ->
      arg1 \s -> pure (callMethod s "to_vec" [])

    ArrayToStream -> arg1 \s -> pure (callMethod s "into_iter" [])
    ArrayToWord   -> arg1 \s ->
      case primTypesOfArgs args of
        [TArray n TBool] ->
          do len <- compileSize OwnContext n MemSize
             pure (mkRustCall (pathExpr (wordName "from_stream_msb"))
                  [ len, callMethod s "into_iter" []])
        _ -> bad

    WordToStream ->
      arg1 \s -> pure (callMethod s "into_iter_bits_msb" [])

    StreamToWord ->
      case primTypesOfArgs args of
        [ TStream (IRSize n) _ ] ->
          do len <- compileSize OwnContext n MemSize
             arg1 \s -> pure (mkRustCall (pathExpr (wordName "from_stream_msb"))
                                         [ len, s ])
        _ -> panic "StreamToWord" ["Input not a stream"]


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

    "#"-> compileAppend args

    -- Literal
    "number" -> pure (callTyTraitMethodWithLen "number")

    -- Zero
    "zero"   -> pure (callTyTraitMethodWithLen "zero")


    -- Ring --
    "+"           -> pure (callTyTraitMethod "add")
    "-"           -> pure (callTyTraitMethod "sub")
    "*"           -> pure (callTyTraitMethod "mut")
    "negate"      -> pure (callTyTraitMethod "negate")
    "^^" ->
      do let fu = pathAddTypeSuffix (rtsName "exp") (primTypeArgs args)
         pure (mkRustCall (pathExpr fu) (primArgs args))

    "fromInteger" -> pure (callTyTraitMethodWithLen "from_integer")

    "/"           -> pure (callTyTraitMethod "div")
    "%"           -> pure (callTyTraitMethod "modulo")
    "toInteger"   -> pure (callTyTraitMethod "to_integer")

    -- Shifts
    ">>>" -> shiftOp False "rotate_right"
    "<<<" -> shiftOp False "rotate_left"
    ">>"  -> shiftOp True  "shift_right"
    ">>$" -> shiftOp False "shift_right_signed"
    "<<"  -> shiftOp True  "shift_left"

    -- Logic
    "&&"          -> pure (callTyTraitMethod "and")
    "||"          -> pure (callTyTraitMethod "or")
    "^"           -> pure (callTyTraitMethod "xor")
    "complement"  -> pure (callTyTraitMethod "complement")

    -- Comparisons
    "=="          -> arg2 (\x y -> pure (binExpr Rust.EqOp x y))
    "!="          -> arg2 (\x y -> pure (binExpr Rust.NeOp x y))
    "<"           -> arg2 (\x y -> pure (binExpr Rust.LtOp x y))
    ">"           -> arg2 (\x y -> pure (binExpr Rust.GtOp x y))
    "<="          -> arg2 (\x y -> pure (binExpr Rust.LeOp x y))
    ">="          -> arg2 (\x y -> pure (binExpr Rust.GeOp x y))


    -- Sequence --
    "!"  -> compileIndexBack args
    "@" -> compileIndexFront args

    "take" ->
      arg1 \x ->
        do mb <- getStreamSizeArg OwnContext args 0
           pure
             case mb of
                Nothing        -> x
                Just (front,_) -> callMethod x "take" [front]

    "drop" ->
      arg1 \x ->
        do (front,_) <- getSizeArg OwnContext args 0
           pure (callMethod x "skip" [ front ])

    "join"      -> compileJoin args
    "split"     -> compileSplit args
    "transpose" -> compileTranspose args
    "reverse"   -> arg1 \x ->
        case primTypeOfResult args of
          TWord {}  -> pure (callMethod x "reverse" [])
          TArray {} -> pure (mkRustCall (pathExpr (rtsName "reverse")) [x])
          _ -> unsupportedPrim "reverse" args

    "fromTo" -> compileFromTo args

    "infFrom" ->
       tyArg1 \t ->
         let fu = pathAddTypeSuffix (rtsName "inf_from") [t]
         in pure (mkRustCall (pathExpr fu) (primLenArgs args ++ primArgs args))

    "infFromThen" ->
       tyArg1 \t ->
         let fu = pathAddTypeSuffix (rtsName "inf_from_then") [t]
         in pure (mkRustCall (pathExpr fu) (primLenArgs args ++ primArgs args))


    -- Polynomial Arithmetic
    "pmult" -> arg2 \x y -> pure (callMethod x "pmult" [y])
    "pmod"  -> arg2 \x y -> pure (callMethod x "pmod" [y])

    _ -> pure (todoExp (Text.unpack name)) -- unsupportedPrim (pp name) args
  where
  callTyTraitMethod method =
    mkRustCall (tyTraitMethod method)
               (primSizeArgs args ++ primArgs args)

  callTyTraitMethodWithLen method =
    mkRustCall (tyTraitMethod method)
               (primLenArgs args ++ primSizeArgs args ++ primArgs args)

  tyTraitMethod method =
      case primTypeArgs args of
        ty : _ -> typeQualifiedExpr ty (simplePath method)
        [] -> panic "tyTraitMethod" ["Expected at least 1 type argument"
                                    , "Method: " ++ show method
                                    ]

  toUSize n =
    do t <- compileType TypeAsParam AsOwned (primTypesOfArgs args !! n)
       let e = primArgs args !! n
       pure $ mkRustCall (typeQualifiedExpr t (simplePath "to_usize")) [e]


  shiftOp withLen op = arg2 \x _y ->
    do y <- toUSize 1
       lenArgs <- if withLen
                    then do let ty = head (primTypesOfArgs args)
                            (:[]) <$> lenParamFor (getTypeElement ty)
                    else pure []
       pure (callMethod x op (lenArgs ++ [y]))

  bad = panic "compilePreludePrim"
          [ "Malformed primitive arguments:"
          , show (pp args)
          ]

  targ2 f =
    case primTypeArgs args of
      x : y : _ -> f x y
      _ -> bad

  arg1 f =
    case primArgs args of
      [a] -> f a
      _ -> bad

  arg2 f =
    case primArgs args of
      [a,b] -> f a b
      _ -> bad


  tyArg1 f =
    case primTypeArgs args of
      t : _ -> f t
      [] -> panic "tyArg1" ["Expected 1 type arguments"]

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

wordName :: Rust.Ident -> RustPath
wordName x = simplePath' [ cryptolCrate, "DWord", x ]




-- | Primitives defined in `Float.cry`
compileCryptolFloatPrim :: Text -> PrimArgs -> Rust RustExpr
compileCryptolFloatPrim _ _ = unsupported "floating point primitve" -- XXX




--------------------------------------------------------------------------------

compileAppend :: PrimArgs -> Rust RustExpr
compileAppend args =
  case primArgs args of
    [x,y] ->
      case primTypeOfResult args of
        TWord {} -> pure (callMethod x "append" [y])
        _ -> pure (callMethod x "chain" [y])
    _ -> unsupportedPrim "#" args


compileJoin :: PrimArgs -> Rust RustExpr
compileJoin args =
  case primTypeOfResult args of
    TWord sz -> stream_to_word sz

    TArray _ elTy ->
      do e <- stream_to_stream elTy
         pure (callMethod e "to_vec" [])

    TStream _ elTy -> stream_to_stream elTy

    _ -> bad
  where
  bad = unsupportedPrim "join" args

  theArgRaw = head (primArgs args)
  theArg =
    case primTypesOfArgs args of
       [TStream {}] -> theArgRaw
       [TArray {}]  -> callMethod theArgRaw "into_iter" []
       _            -> panic "compileJoin" ["Unexpected type of input"]

  stream_to_word sz =
    case sz of

      IRFixedSize 0 ->
        pure
          (mkRustCall (pathExpr (wordName "zero")) [ litExpr (mkUSizeLit 0) ])

      _ ->
        do (parts,_) <- getSizeArg OwnContext args 0
           (each,_)  <- getSizeArg OwnContext args 1
           let fun = wordName "join"
           pure (mkRustCall (pathExpr fun) [parts, each, theArg])

  stream_to_stream elTy = pure (mkRustCall (pathExpr (rtsName fun)) [theArg])
    where
    fun =
      case elTy of
        TBool -> "join_words"
        _     -> "join_vecs"


compileSplit :: PrimArgs -> Rust RustExpr
compileSplit args =
  do theArg <- case primArgs args of
                 [a] -> pure a
                 _   -> bad
     (each,_) <- getSizeArg OwnContext args 1
     case primTypesOfArgs args of
       [TWord {}] -> fixOut (callMethod theArg "into_iter_words_msb" [each])
       [TArray {}]  -> fromStream each (callMethod theArg "into_iter" [])
       [TStream {}] -> fromStream each theArg
       _ -> bad
  where
  fromStream each theArg =
    case getTypeElement (primTypeOfResult args) of
      TWord {} -> fixOut (mkRustCall (pathExpr (rtsName "split_bits"))
                                                        [ each, theArg ])
      _ -> fixOut (mkRustCall (pathExpr (rtsName "split")) [ each, theArg ])

  fixOut o =
    case primTypeOfResult args of
      TStream {} -> pure o
      TArray {}  -> pure (callMethod o "to_vec" [])
      _          -> bad

  bad  = unsupportedPrim "split" args


compileTranspose :: PrimArgs -> Rust RustExpr
compileTranspose args =
  do fun <- case primTypeOfResult args of
              TArray _ (TWord {})  -> pure "transpose_word"
              TArray _ (TArray {}) -> pure "transpose_vec"
              _ -> unsupportedPrim "transpose" args
     pure (mkRustCall (pathExpr (rtsName fun))
                      (primSizeArgs args ++ primArgs args))


compileIndexFront :: PrimArgs -> Rust RustExpr
compileIndexFront args =
  case primTypeArgs args of
    [elT,ixT] ->
      do fu <- case primTypesOfArgs args of
                 [TStream {},_] ->
                    pure (pathAddTypeSuffix (rtsName "index_stream") [elT,ixT])
                 [TArray {},_] ->
                    pure (pathAddTypeSuffix (rtsName "index_vec") [elT,ixT])
                 [TWord {},_] ->
                    pure (pathAddTypeSuffix (rtsName "index_word") [ixT])
                 _ -> bad
         pure (mkRustCall (pathExpr fu) (primArgs args))
    _ -> bad
  where
  bad :: Rust a
  bad = unsupportedPrim "@" args

compileIndexBack :: PrimArgs -> Rust RustExpr
compileIndexBack args =
  case primTypeArgs args of
    [elT,ixT] ->
      do (mbLen, fu) <-
          case primTypesOfArgs args of
            [TStream {},_] ->
               do (len,_) <- getSizeArg OwnContext args 0
                  pure ( [len]
                       , pathAddTypeSuffix (rtsName "index_stream_back")
                                                                    [elT,ixT])

            [TArray {},_] ->
               pure ([], pathAddTypeSuffix (rtsName "index_vec_back") [elT,ixT])

            [TWord {},_] ->
               pure ([], pathAddTypeSuffix (rtsName "index_word_back") [ixT])
            _ -> bad
         pure (mkRustCall (pathExpr fu) (mbLen ++ primArgs args))
    _ -> bad
  where
  bad :: Rust a
  bad = unsupportedPrim "!" args



compileFromTo :: PrimArgs -> Rust RustExpr
compileFromTo args =
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


