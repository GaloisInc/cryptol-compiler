module Cryptol.Compiler.Rust.CodeGen
  ( genModule
  , ExtModule(..)
  , GenInfo(..)
  )  where

import Data.Set qualified as Set
import Data.Maybe (catMaybes)
import Control.Monad(forM, mapAndUnzipM, foldM)

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Utils.Ident qualified as Cry

-- import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.Free
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.CompileSize
import Cryptol.Compiler.Rust.CompileType
import Cryptol.Compiler.Rust.CompileTrait
import Cryptol.Compiler.Rust.CompilePrim


data ExprContext =
    OwnContext    -- ^ We are generating an ownded expression
  | BorrowContext -- ^ We are generating a borrowed expression

-- | Generate Rust code for a Cryptol IR expression, creating a block if
doGenExpr :: ExprContext -> Expr -> Rust RustExpr
doGenExpr how e =
  do (stmts,expr) <- genExpr how e
     pure (blockExprIfNeeded stmts expr)

-- | Generate a Rust block corresponding to a Cryptol IR expression
genBlock :: ExprContext -> Expr -> Rust RustBlock
genBlock how e = uncurry block' <$> genExpr how e


-- | Length arguments for the `Length` trait
genCallLenArgs :: Call -> Rust [RustExpr]
genCallLenArgs call =
  traverse lenParamFor
    [ arg | (t, arg) <- ftTypeParams funTy `zip` typeArgs
          , t `Set.member` needLen
    ]
  where
  funTy = ircFunType call
  needLen = Set.unions (map traitNeedsLen (ftTraits funTy))
  typeArgs =
    case ircFun call of
      IRFunVal _ -> []
      IRTopFun tf -> irtfTypeArgs tf

-- | Size arguments for a call
genCallSizeArgs :: Call -> Rust [RustExpr]
genCallSizeArgs call =
  case ircFun call of
    IRFunVal _ -> pure []
    IRTopFun tf -> traverse (uncurry compileSize) (irtfSizeArgs tf)

-- | Normal arguments for a call
genCallArgs :: ExprContext -> Call -> Rust ([RustStmt], [RustExpr])
genCallArgs how call =
  do (stmtss,es) <- mapAndUnzipM (genExpr how) (reverse (ircArgs call))
     pure (concat (reverse stmtss), reverse es)

-- | Returns an owned result.
genCall :: Call -> Rust (PartialBlock RustExpr)
genCall call =

  case ircFun call of

    -- Closures don't have polymorphic arguments, and so shouldn't have
    -- size or lenght arguments. Captured length arguments should be in scope.
    IRFunVal fnIR -> pure ([], todoExp "Call closure")
{-
      do fnExpr       <- doGenExpr BorrowContext fnIR
         -- XXX: is Borrow sufficient here?

         (stmts,args) <- genCallArgs call
         pure (stmts, mkRustCall fnExpr args)
-}

    IRTopFun tf ->
      do typeArgs     <- traverse (compileType AsOwned) (irtfTypeArgs tf)
         lenArgs      <- genCallLenArgs call
         szArgs       <- genCallSizeArgs call
         name         <- lookupFunName (irtfName tf)

         case name of
           Left prim ->
              do isCon <- primIsConstructor prim
                 let ctx = if isCon then OwnContext else BorrowContext
                 (stmts,args) <- genCallArgs ctx call
                 rexpr <- compilePrim prim
                            PrimArgs
                              { primTypesOfArgs  = ircArgTypes call
                              , primTypeOfResult = ircResType call
                              , primTypeArgs     = typeArgs
                              , primLenArgs      = lenArgs
                              , primSizeArgs     = szArgs
                              , primArgs         = args
                              }
                 pure (stmts,rexpr)

           Right path ->
             do (stmts,args) <- genCallArgs BorrowContext call
                let fun = pathExpr (pathAddTypeSuffix path typeArgs)
                    allArgs = lenArgs ++ szArgs ++ args
                pure (stmts, mkRustCall fun allArgs)

-- | From a Cryptol IR expression, generate a Rust expressions along with
--   a set of Rust statements that contextualize it, such as let bindings
-- IMPORTANT: we should be generating code "backwards" meaning first generate
-- the continuation, and then the stuff using the continuation.  We do this
-- becasue we are also doing an analysis of what is used in the continuation
-- and how many times.  Use `withSameCont` for things that share a continuation.
genExpr :: ExprContext -> Expr -> Rust (PartialBlock RustExpr)
genExpr how (IRExpr e0) =
  case e0 of
    IRVar (IRName name ty) ->
      do (isLocal,isLastUse,rexpr) <- lookupNameId name
         justExpr <$>
           case how of
             OwnContext
               | isLocal ->
                 pure
                 if isLastUse
                    then rexpr
                    else callMethod rexpr "clone" []

               | otherwise -> pure (callMethod rexpr "clone_arg" [])

             BorrowContext
               | isLocal   -> pure (callMethod rexpr "as_arg" [])
               | otherwise -> pure rexpr

    IRCallFun call ->
      do (stmts, rexpr) <- genCall call
         pure
           ( stmts
           , case how of
               OwnContext    -> rexpr
               BorrowContext -> callMethod (addrOf rexpr) "as_arg" []
           )

    IRIf eTest eThen eElse ->
      do res <- withSameCont (map (genBlock how) [ eThen, eElse ])
         let rThen = res !! 0
             rElse = res !! 1
         (stmts,rTest) <- genExpr OwnContext eTest
         pure (stmts, rustIf rTest rThen rElse)

    IRLet (IRName name _) eBound eIn ->
      do (rname,rstmts,rexpr) <-
           bindLocalLet name \rname ->
             do (rstmts,e) <- genExpr how eIn
                pure (rname,rstmts,e)

         eBound' <- doGenExpr OwnContext eBound
         let ty = Nothing
             letBind = Rust.Local (identPat rname) ty (Just eBound') [] ()

         pure (letBind : rstmts, rexpr)

    IRClosure {} -> unsupported "Closure" -- pure (justExpr (todoExp (show (pp call))))

    IRLam {} -> pure ([], todoExp "Lambda Expression")
  {-
    justExpr <$>
      do  let args' = irNameName <$> args
        args'' <- bindLocal False addLocalVar `traverse` args'
          (lamStmt, lamE) <- genExpr expr
          pure $ mkClosure args'' lamStmt lamE
          -}



    IRStream streamExpr -> genStream how streamExpr


        -- | Generate a RustItem corresponding to a function declaration.
  --   Returns `Nothing` if the declaration is for an IR primitive.
genFunDecl :: FunDecl -> Rust (Maybe RustItem)
genFunDecl decl =
  case irfDef decl of
  -- TODO: maybe this would be a good place to check that the set
  --       of implemented primitives is complete?
  IRFunPrim -> pure Nothing
  IRFunDef argNames expr ->
    do isLocal <- isFunNameLocal (irfName decl)
       if isLocal then doCompile argNames expr else pure Nothing

  where
  doCompile argNames expr =
    do -- doIO (print $ pp decl)
       resetFunLocalNames
       name <- bindFun (irfName decl)
       let ft = irfType decl

       localScope
         do -- Type parameters
            tNames <- mapM (bindLocal addLocalType) (ftTypeParams ft)

            -- Traits
            (needLen,quals') <- mapAndUnzipM compileTrait (ftTraits ft)
            let allNeedLen = Set.unions needLen
                lenParamTs = filter (`Set.member` allNeedLen) (ftTypeParams ft)
            cryTypeConstraints <- mapM isCryType (ftTypeParams ft)
            let quals = cryTypeConstraints ++ quals'

            -- Lenght parameters for Traits that need them
            lParams <- forM lenParamTs \tp ->
              do i <- bindLocal addLocalLenghtParam tp
                 t <- lenParamType tp
                 pure (i,t)

            -- Size parameters
            sParams <- forM (ftSizeParams ft) \sp ->
              do i <- bindLocal addLocalType (irsName sp)
                 pure (i, compileSizeType (irsSize sp))

            -- Normal parameters
            nParams <- forM (argNames `zip` ftParams ft) \(arg,ty) ->
              do i <- bindLocal (addLocalVar False) arg
                 t <- compileType AsArg ty
                 pure (i, t)

            returnTy  <- compileType AsOwned (ftResult ft)

            funExpr   <- genBlock OwnContext expr

            let tyParams    = [ Rust.TyParam [] i [] Nothing () | i <- tNames ]
                params      = lParams ++ sParams ++ nParams
                lifetimes   = []
                whereClause = Rust.WhereClause quals ()
                generics    = mkGenerics lifetimes tyParams whereClause
            pure (Just (mkFnItem name generics params returnTy funExpr))


-- | Given a set of FunDecls, make a Rust SourceFile
genSourceFile ::
  Cry.ModName ->
  [FunDecl] ->
  Rust (ExtModule, Rust.SourceFile ())
genSourceFile m decls =
  do  -- doIO (print $ vcat $ map pp decls)

      fnItems <- catMaybes <$> (genFunDecl `traverse` decls)
      let imports = [ mkUseGlob [ cryptolCrate, "trait_methods"]
                    ]
          modName = modNameToRustModName m
          file = Rust.SourceFile (Just modName) [] (imports ++ fnItems)
      names <- getFunNames
      let extMod =
            ExtModule { extModuleName  = Rust.mkIdent modName
                      , extModuleNames = names
                      }
      pure (extMod, file)

genModule ::
  GenInfo ->
  [FunDecl] ->
  IO (ExtModule, Rust.SourceFile ())
genModule gi ds = runRustM gi (genSourceFile (genCurModule gi) ds)

--------------------------------------------------------------------------------

genStream :: ExprContext -> StreamExpr -> Rust (PartialBlock RustExpr)
genStream ctxt sexpr =
  do (extStrStmts, binds) <- foldM doExtStream ([],[]) (irsExterns sexpr)
     (initStmts,initE) <- genExpr OwnContext (irsInit sexpr)
     let extStreamSmts = concat (initStmts : extStrStmts)
     undefined


      -- S { index = 0, history = init, nonStream, stream }

     undefined

  -- For each extrenal stream we need a new type paramer: XS: Stream<a>
  -- For each type parameter we depend on (in extrenal stream + non-steram and next)
  -- we need a parameter with all the constraints that we have in the function
  where
  extNonStream = freeLocals (freeNames (irsNext sexpr))

  doExtStream (ss,binds) (x,e) =
    do (xs,y) <- genExpr OwnContext e
       pure (xs : ss, (x,y) : binds)

  nextBody = undefined
  -- i = self.index
  -- x = x.self.clone() -- for all locals
  --
  -- hist(x) ~> self.history[(i + hist_size - x) % hist_size]
  -- head(xs) ~> self.xs.next()

--------------------------------------------------------------------------------

