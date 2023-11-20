module Cryptol.Compiler.Rust.CompileStream where

import Data.Set qualified as Set
import Data.List(intercalate)

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

     let histLen =
            case typeOf (irsInit sexpr) of
              TArray (IRFixedSize i) _ -> i
              _ -> panic "genStream" ["init fields is not a know fixed array"]

     -- Adds the locals corresponding to the captured streams.
     extStreamInfo <- zipWithM streamTypeVar [1..] (irsExterns sexpr)
     gen           <- makeGenerics allTypeVars extStreamInfo

     -- Generate the step function
     (usedLens,usedSizes,step) <-
        inStreamClosure OwnContext
           (uncurry blockExprIfNeeded <$> ?genExpr OwnContext (irsNext sexpr))
     forM_ (irsExterns sexpr) \(s,_) -> removeLocalLet (irNameName s)

     (initStmts,initE) <- ?genExpr OwnContext (irsInit sexpr)
     (extStrStmts, binds) <- foldM doExtStream ([],[]) (irsExterns sexpr)
     let stmts = concat (initStmts : reverse extStrStmts)

     extVars  <- mapM doExtVar extNonStream
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
          [ (extStrLabel info, extStrType info, bind)
          | (info,bind) <- zip extStreamInfo (reverse binds)
          ]
     pure (stmts, streamMacro gen elTy histLen capture initE step)


  where
  doExtStream (ss,binds) (_,e) =
    do (xs,y) <- ?genExpr OwnContext e
       pure (xs : ss, y : binds)

  doExtVar x =
    do r <- lookupNameRustIdent (irNameName x)
       t <- compileType TypeAsStored AsOwned (typeOf x)
       (stmts,e) <- ?genExpr OwnContext (IRExpr (IRVar x))
       unless (null stmts) (panic "doExtVar" ["Unexpected statements"])
       pure (r,t,e)

  -- All type variables mentioned in the closure
  -- (XXX: also variables for the types of external functinos)
  allTypeVars  =
    Set.toList (
      Set.unions
       $ freeValTypeVars (irsType sexpr)
       : [ freeValTypeVars (typeOf x) | x <- extNonStream ]
      ++ [ freeValTypeVars (typeOf s) | (s,_) <- irsExterns sexpr ]
    )

  -- Free variables that need to be packed with the stream.
  extNonStream =
    Set.toList (freeLocals (freeNames (irsNext sexpr))
                  `Set.difference` Set.fromList (map fst (irsExterns sexpr)))


-- | Information about an external stream
data ExtStreamInfo = ExtStreamInfo
  { extStrLabel   :: Rust.Ident             -- ^ Value name of the stream
  , extStrTyParam :: Rust.Ident             -- ^ The type parameter for it's type
  , extStrType    :: RustType               -- ^ The type parameter as a type
  , extStrTraits  :: [RustPath]             -- ^ Constraints on the type param
  }

-- | Information about the name and type of an external stream vairable
-- Note that this adds the stream variable to the current scope, and
-- we should remove it at the end.
streamTypeVar :: Int -> (Name,Expr) -> Rust ExtStreamInfo
streamTypeVar i (s,_) =
  case irNameType s of
    TStream _ elT ->
      do ruT <- compileType TypeAsStored AsOwned elT
         let ident = Rust.mkIdent ("SI" ++ show (i::Int))
             ty    = pathType (simplePath ident)
             path  = streamTraitPath ruT
         x <- bindLocal (addLocalVar (Just LocalOrClosure)) (irNameName s)
         pure
           ExtStreamInfo
             { extStrLabel   = x
             , extStrTyParam = ident
             , extStrType    = ty
             , extStrTraits  = [path]
             }

    _ -> panic "streamTypeVar" ["Extern stream is no a stream"]





-- | Make type parameters and their constraints
makeGenerics ::
  [Cry.TParam] ->
  [ExtStreamInfo] ->
  Rust [(Rust.Ident, [RustPath])]
makeGenerics allTypeVars svars =
  do extTPS <- forM allTypeVars \a ->
                 do i     <- lookupTParam a
                    bnds  <- getTypeBounds a
                    tyBnd <- isCryType a
                    pure (i, map fromPred (tyBnd : bnds))

     let strTPS  = [ (extStrTyParam e, extStrTraits e) | e <- svars ]

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
  Integer ->
  [ (Rust.Ident, RustType, RustExpr) ] ->
  RustExpr ->
  RustExpr ->
  RustExpr
streamMacro gen elT histLen capture initE stepE =
    callMacro' (simplePath' [cryptolCrate, "stream"])
  $ commas
    [ "forall"  ~> delim [ [ident x, colon,
                                  delim [ [inter Rust.NtPath p] | p <- cs ] ]
                         | (x,cs) <- gen
                         ]
    , "element" ~> inter Rust.NtTy elT
    , "history" ~> num histLen

    , "capture" ~> delim [ [ ident x, colon, inter Rust.NtTy t,
                                            eq, inter Rust.NtExpr e ]
                         | (x,t,e) <- capture
                         ]

    , "init"    ~> inter Rust.NtExpr initE
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

