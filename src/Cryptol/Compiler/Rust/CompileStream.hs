module Cryptol.Compiler.Rust.CompileStream where

import Data.Set qualified as Set

import Control.Monad(forM, zipWithM, forM_)
import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Quote qualified as Rust

import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.Free

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.CompileTrait
import Cryptol.Compiler.Rust.CompileType

type CompExpr =
        (?genExpr :: ExprContext -> Expr -> Rust (PartialBlock RustExpr))


-- | The ExprContext parameter indicates if the closure should capture
-- external variables by reference or by value.
genStream ::
  CompExpr =>
  ExprContext -> StreamExpr -> Rust (PartialBlock RustExpr)
genStream rctxt sexpr =
  do elTy <- case irsType sexpr of
               TStream _ elT -> compileType TypeAsStored AsOwned elT
               _ -> panic "genStream" ["Not a stream"]

     let (histLen, histType) =
            case typeOf (irsInit sexpr) of
              TArray (IRFixedSize i) _ -> (i, fixedArrayOfType elTy i)
              _ -> panic "genStream" ["init fields is not a know fixed array"]

     -- Adds the locals corresponding to the captured streams.
     extStreamInfo <- zipWithM streamTypeVar [1..] (irsExterns sexpr)
     gen           <- makeGenerics allTypeVars extStreamInfo

     let (structDecl,strTy) = makeStructDecl gen histType extStreamInfo
     iterDecl <- makeIterImpl gen histLen strTy elTy (irsNext sexpr)

     forM_ (irsExterns sexpr) \(s,_) -> removeLocalLet (irNameName s)


     let stmts =
           [ Rust.ItemStmt structDecl ()    -- declare struct
           , Rust.ItemStmt iterDecl  ()
           ]

     -- XXX: TMP
     pure ([], Rust.BlockExpr [] (block stmts) ())


{-
  do (extStrStmts, binds) <- foldM doExtStream ([],[]) (irsExterns sexpr)
     (initStmts,initE)    <- genExpr OwnContext (irsInit sexpr)
     let extStreamSmts = concat (initStmts : extStrStmts)



     undefined


      -- S { index = 0, history = init, nonStream, stream }

     undefined

-}
  where


  -- All type variables mentioned in the closure
  allTypeVars  =
    Set.toList (
      Set.unions
       $ freeValTypeVars (irsType sexpr)
       : [ freeValTypeVars (typeOf x) | x <- Set.toList extNonStream ]
      ++ [ freeValTypeVars (typeOf s) | (s,_) <- irsExterns sexpr ]
    )

  -- Free variables that need to packed with the stream.
  extNonStream = freeLocals (freeNames (irsNext sexpr))

  doExtStream (ss,binds) (x,e) =
    do (xs,y) <- ?genExpr OwnContext e
       pure (xs : ss, (x,y) : binds)


-- | Information about an external stream
data ExtStreamInfo = ExtStreamInfo
  { extStrLabel   :: Rust.Ident             -- ^ Value name of the stream
  , extStrTyParam :: RustTyParam            -- ^ The type parameter for it's type
  , extStrType    :: RustType               -- ^ The type parameter as a type
  , extStrTraits  :: RustWherePredicate     -- ^ Constraints on the type param
  }


-- | Generate a struct for the stream's state.
-- We also return the type of the sturct, in terms of the given generics.
--
-- XXX: we should abstract over history and index, and make sure
-- they don't clash with other names.
makeStructDecl ::
  RustGenerics -> RustType -> [ExtStreamInfo] -> (RustItem,RustType)
makeStructDecl gen histType extStreamInfo =
  ( Rust.StructItem [] Rust.InheritedV tyName varData sGen ()
  , strTy
  )
  where
  fields =
    [ structField "index"    (simpleType "usize")
    , structField "history"  histType
    ] ++
    [ structField (extStrLabel ex) (extStrType ex) | ex <- extStreamInfo ]
    -- ++ XXX: capture external non-stream things

  tyName  = "S"
  varData = Rust.StructD fields ()

  -- we don't need the constraints in the stuct decl
  (strTy,sGen) =
    case gen of
      Rust.Generics lf tps _ctrs a ->
        ( let ps = [ pathType (simplePath i) | Rust.TyParam _ i _ _ _ <- tps ]
          in pathType (pathAddTypeSuffix (simplePath tyName) ps)

        , Rust.Generics lf tps (Rust.WhereClause [] ()) a
        )


-- | Make the generics parameters for a stream struct declaration.
makeGenerics ::
  [Cry.TParam] ->
  [ExtStreamInfo] ->
  Rust RustGenerics
makeGenerics allTypeVars svars =
  do (extTPS,extBNDS) <-
       unzip <$> forM allTypeVars \a ->
                   do i     <- lookupTParam a
                      bnds  <- getTypeBounds a
                      tyBnd <- isCryType a
                      pure (tyParam i, tyBnd : bnds)

     let strTPS  = map extStrTyParam svars
         strBNDS = map extStrTraits  svars

     -- XXX: if there are external capturing things that are functions,
     -- those should be treated like the streams:
     -- we generate a new type variable
     -- for each, and add `Fn((xs) -> y)` constraint on each.
     let (fnTPS, fnBNDS) = ([],[])

     let allTPS  = strTPS ++ fnTPS ++ extTPS
         allBNDS = concat (strBNDS : fnBNDS : extBNDS)

     -- XXX: we should add a liftime when we are borrowing if any of
     -- the externally captured locals are references
     pure (Rust.Generics [] allTPS (Rust.WhereClause allBNDS ()) ())


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
             b     = Rust.PolyTraitRef [] (Rust.TraitRef path) ()
             bound = Rust.BoundPredicate [] ty
                            [Rust.TraitTyParamBound b Rust.None ()] ()
         x <- bindLocal (addLocalVar True) (irNameName s)
         pure
           ExtStreamInfo
             { extStrLabel   = x
             , extStrTyParam = tyParam ident
             , extStrType    = ty
             , extStrTraits  = bound
             }

    _ -> panic "streamTypeVar" ["Extern stream is no a stream"]



makeIterImpl ::
  CompExpr =>
  RustGenerics {- ^ Poly parameters -} ->
  Integer      {- ^ Size of history -} ->
  RustType     {- ^ Type of the stream -} ->
  RustType     {- ^ Type of the stream elemtn -} ->
  Expr         {- ^ How to compute the next element -} ->
  Rust RustItem
makeIterImpl gen histLen strT elT def =
  do (stmts,val) <- ?genExpr OwnContext def
     let rest = [ localLet resultVar Nothing val
                ]
     pure $
       mkImplTrait gen (iteratorWithTypePath elT) strT
          [ implType "Item" elT
          , implMethod "next" noGenerics nextSig undefined
                        -- (block' stmts (pathExpr (simplePath "r
          ]

  where
  resultVar = "result"

  nextSig =
     methodSig [selfRef Rust.Mutable]
         (Just (pathType (pathAddTypeSuffix (simplePath "Option") [elT])))

