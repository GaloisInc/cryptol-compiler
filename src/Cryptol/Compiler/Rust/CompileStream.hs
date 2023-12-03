module Cryptol.Compiler.Rust.CompileStream where

import Data.Set qualified as Set
import Data.List(intercalate,partition)

import Control.Monad(forM, zipWithM, forM_, foldM,unless)
import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.Free

import Cryptol.Compiler.PP
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.CompileTrait
import Cryptol.Compiler.Rust.CompileSize
import Cryptol.Compiler.Rust.CompileType

type CompExpr =
        (?genExpr :: ExprContext -> Expr -> Rust (PartialBlock RustExpr))


genStream :: CompExpr => StreamExpr -> Rust (PartialBlock RustExpr)
genStream sexpr =
  do elTy <- case irsType sexpr of
               TStream _ elT -> compileType TypeAsStored AsOwned elT
               _ -> panic "genStream" ["Not a stream"]


     -- Adds the locals corresponding to the captured streams.
     extStreamInfo <- zipWithM streamTypeVar [1..]
                            ( [ (x, Nothing) | x <- extStreamVars ]
                           ++ [ (x, Just e)  | (x,e) <- irsExterns sexpr ]
                            )
     gen           <- makeGenerics allTypeVars extStreamInfo

     -- Generate the step function
     (usedLens,usedSizes,step) <-
        inStreamClosure OwnContext
           (uncurry blockExprIfNeeded <$> ?genExpr OwnContext (irsNext sexpr))
     forM_ extStreamInfo \s ->
       case stoDef s of
         Just _ -> removeLocalLet (irNameName (stoName s))
         _      -> pure ()

     -- Initialization code for recursive streams
     (initStmts,mbRec) <-
       case irsRec sexpr of
         NonRecStream -> pure ([], Nothing)
         RecStream ini ->
           do let histLen =
                    case typeOf ini of
                       TArray (IRFixedSize i) _ -> i
                       _ -> panic "genStream"
                              ["init fields is not a know fixed array"]
              (stmts,e) <- ?genExpr OwnContext ini
              pure (stmts, Just (histLen,e))

     (extStrStmts, binds) <- foldM doExtStream ([],[]) extStreamInfo
     let stmts = concat (initStmts : reverse extStrStmts)

     extVars  <- mapM doExtVar extNonStreamVars
     let extSizes = [ ( i, compileSizeType OwnContext sz
                      , case sz of
                          MemSize -> e
                          LargeSize -> callMethod e "clone" []
                      )
                    | (i,sz) <- usedSizes
                    , let e = pathExpr (simplePath i)
                    ]
     extLens <- forM usedLens \(i,tp) ->
                 do t <- lenParamType tp
                    pure (i,t, pathExpr (simplePath i))

     let capture =
          extLens ++
          extSizes ++
          extVars ++
          [ (stoStrLabel info, stoStrType info, bind)
          | (info,bind) <- zip extStreamInfo (reverse binds)
          ]
     pure (stmts, streamMacro gen elTy capture mbRec step)


  where
  doExtStream (ss,binds) s =
    do (xs,y) <- ?genExpr OwnContext
                    case stoDef s of
                      Just e  -> e
                      Nothing -> IRExpr (IRVar (stoName s))
       pure (xs : ss, y : binds)

  doExtVar x =
    do r <- lookupNameRustIdent (irNameName x)
       t <- compileType TypeAsStored AsOwned (typeOf x)
       (stmts,e) <- ?genExpr OwnContext (IRExpr (IRVar x))
       unless (null stmts) (panic "doExtVar" ["Unexpected statements"])
       pure (r,t,e)

  -- All type variables mentioned in the closure
  -- (XXX: also variables for the types of external functions)
  allTypeVars  =
    Set.toList (
      Set.unions
       $ freeValTypeVars (irsType sexpr)
       : [ freeValTypeVars (typeOf x) | x <- extNonStreamVars ]
      ++ [ freeValTypeVars (typeOf x) | x <- extStreamVars ]
      ++ [ freeValTypeVars (typeOf s) | (s,_) <- irsExterns sexpr ]
    )

  -- Free variables that need to be packed with the stream.
  (extStreamVars, extNonStreamVars) =
    let allVars   = freeLocals (freeNames (irsNext sexpr))
        bound     = Set.fromList (map fst (irsExterns sexpr))
        isStr x   = case irNameType x of
                      TStream {} -> True
                      _          -> False
        varList   = Set.toList (allVars `Set.difference` bound)
    in partition isStr varList


-- | Information about a stream stored in a stream closure.
data StoredStream = StoredStream
  { stoName       :: Name        -- ^ Original name of the stored stream
  , stoDef        :: Maybe Expr  -- ^ Definition for stream, if mutable
  , stoStrLabel   :: Rust.Ident  -- ^ Value name of the stream
  , stoStrTyParam :: Rust.Ident  -- ^ The type parameter for it's type
  , stoStrType    :: RustType    -- ^ The type parameter as a type
  , stoStrTraits  :: [RustPath]  -- ^ Constraints on the type param
  }

-- | Information about the name and type of an external stream vairable
-- Note that this adds the stream variable to the current scope, and
-- we should remove it at the end.
streamTypeVar :: Int -> (Name,Maybe Expr) -> Rust StoredStream
streamTypeVar i (s,e) =
  case irNameType s of
    TStream _ elT ->
      do ruT <- compileType TypeAsStored AsOwned elT
         let ident = Rust.mkIdent ("SI" ++ show (i::Int))
             ty    = pathType (simplePath ident)
             path  = streamTraitPath ruT
         x <- case e of
                Just _ ->
                  bindLocal (addLocalVar (Just LocalOrClosure)) (irNameName s)
                Nothing -> lookupNameRustIdent (irNameName s)
         pure
           StoredStream
             { stoName       = s
             , stoDef        = e
             , stoStrLabel   = x
             , stoStrTyParam = ident
             , stoStrType    = ty
             , stoStrTraits  = [path]
             }

    _ -> panic "streamTypeVar" ["Extern stream is no a stream"]





-- | Make type parameters and their constraints
makeGenerics ::
  [Cry.TParam] ->
  [StoredStream] ->
  Rust [(Rust.Ident, [RustPath])]
makeGenerics allTypeVars svars =
  do extTPS <- forM allTypeVars \a ->
                 do i     <- lookupTParam a
                    bnds  <- getTypeBounds a
                    tyBnd <- isCryType a
                    pure (i, map fromPred (tyBnd : bnds))

     let strTPS  = [ (stoStrTyParam e, stoStrTraits e) | e <- svars ]

     -- XXX: if there are external capturing things that are functions,
     -- those should be treated like the streams:
     -- we generate a new type variable
     -- for each, and add `Fn((xs) -> y)` constraint on each.
     let fnTPS = []

     -- XXX: we should add a liftime when we are borrowing if any of
     -- the externally captured locals are references
     pure (strTPS ++ fnTPS ++ extTPS)

  where
  fromPred pr =
    case pr of
      Rust.BoundPredicate _ _
        [Rust.TraitTyParamBound
          (Rust.PolyTraitRef _ (Rust.TraitRef p) _) _ _] _ -> p
      _ -> panic "makeGenerics" ["Unexpected type bound"]


streamMacro ::
  [(Rust.Ident, [RustPath])] ->
  RustType ->
  [ (Rust.Ident, RustType, RustExpr) ] ->
  Maybe (Integer, RustExpr) ->
  RustExpr ->
  RustExpr
streamMacro gen elT capture mbRec stepE =
    callMacro' (simplePath' [cryptolCrate, "stream"])
  $ commas $
    [ "forall"  ~> delim [ [ident x, colon,
                                  delim [ [inter Rust.NtPath p] | p <- cs ] ]
                         | (x,cs) <- gen
                         ]
    , "element" ~> inter Rust.NtTy elT
    ] ++
    [ f
    | Just (histLen,initE) <- [mbRec]
    , f <- [ "history" ~> num histLen
           , "init"    ~> inter Rust.NtExpr initE
           ]
    ]
    ++
    [ "capture" ~> delim [ [ ident x, colon, inter Rust.NtTy t,
                                            eq, inter Rust.NtExpr e ]
                         | (x,t,e) <- capture
                         ]
    , "step"    ~> Rust.Stream [ tok Rust.Pipe
                               , ident "this"
                               , tok Rust.Pipe
                               , inter Rust.NtExpr stepE
                               ]
     ]
  where
  k ~> v  = [ ident k, eq, v ]
  tok     = Rust.Tree . Rust.Token dummySpan
  ident   = tok . Rust.IdentTok
  commas  = Rust.Stream . intercalate [tok Rust.Comma]
  colon   = tok Rust.Colon
  eq      = tok Rust.Equal
  delim   = Rust.Tree
          . Rust.Delimited dummySpan Rust.Bracket
          . commas

  num x   = tok (Rust.LiteralTok (Rust.IntegerTok (show x)) Nothing)
  inter f = tok . Rust.Interpolated . f . fmap (const dummySpan)

