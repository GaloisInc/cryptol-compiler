module Cryptol.Compiler.Rust.CodeGen
  ( genModule
  , ExtModule(..)
  , GenInfo(..)
  )  where

import Data.Set qualified as Set
import Data.Maybe (catMaybes)
import Control.Monad(forM, mapAndUnzipM)

import Language.Rust.Syntax qualified as Rust

import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(unsupported)
import Cryptol.Compiler.IR.Cryptol
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
    IRFunVal fnIR -> unsupported "Call closure"
{-
      do fnExpr       <- doGenExpr BorrowContext fnIR
         -- XXX: is Borrow sufficient here?

         (stmts,args) <- genCallArgs call
         pure (stmts, mkRustCall fnExpr args)
-}

    IRTopFun tf ->
      do typeArgs     <- traverse compileType (irtfTypeArgs tf)
         lenArgs      <- genCallLenArgs call
         szArgs       <- genCallSizeArgs call
         name         <- lookupFunName (irtfName tf)

         case name of
           Left prim ->
              do let ctx = if primIsConstructor prim then OwnContext
                                                     else BorrowContext
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
genExpr :: ExprContext -> Expr -> Rust (PartialBlock RustExpr)
genExpr how (IRExpr e0) =
  case e0 of
    IRVar (IRName name _) ->
      do (isLocal,isLastUse,rexpr) <- lookupNameId name
         pure $ justExpr
           case how of
             OwnContext
               | isLocal ->
                 if isLastUse
                    then rexpr
                    else callMethod rexpr "clone" []
               | otherwise -> callMethod rexpr "clone" []
                -- XXX: unless copy type
             BorrowContext
               | isLocal   -> addrOf rexpr -- XXX: unless copy type
               | otherwise -> rexpr

         -- pure (justExpr newExpr)

    IRCallFun call ->
      do (stmts, rexpr) <- genCall call
         pure
           ( stmts
           , case how of
               OwnContext    -> rexpr
               BorrowContext -> addrOf rexpr
           )

    IRIf eTest eThen eElse ->
      do ~[ rThen, rElse ] <- withSameCont (map (genBlock how) [ eThen, eElse ])
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

    IRLam {} -> unsupported "Lambda"
  {-
    justExpr <$>
      do  let args' = irNameName <$> args
        args'' <- bindLocal False addLocalVar `traverse` args'
          (lamStmt, lamE) <- genExpr expr
          pure $ mkClosure args'' lamStmt lamE
          -}

    IRStream {} -> unsupported "Stream"



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
    do doIO (print $ pp decl)
       resetFunLocalNames
       name <- bindFun (irfName decl)
       let ft = irfType decl

       localScope
         do -- Type parameters
            tNames <- mapM (bindLocal addLocalType) (ftTypeParams ft)

            -- Traits
            (needLen,quals) <- mapAndUnzipM compileTrait (ftTraits ft)
            let allNeedLen = Set.unions needLen
                lenParamTs = filter (`Set.member` allNeedLen) (ftTypeParams ft)

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
                 t <- compileType ty
                 pure (i, refType t) -- XXX: Copy params, polymorphic stuff

            returnTy  <- compileType (ftResult ft)

            funExpr   <- genBlock OwnContext expr

            let tyParams    = [ Rust.TyParam [] i [] Nothing () | i <- tNames ]
                params      = lParams ++ sParams ++ nParams
                lifetimes   = []
                whereClause = Rust.WhereClause quals ()
                generics    = mkGenerics lifetimes tyParams whereClause
            pure (Just (mkFnItem name generics params returnTy funExpr))


-- | Given a set of FunDecls, make a Rust SourceFile
genSourceFile :: [FunDecl] -> Rust (Rust.SourceFile ())
genSourceFile decls =
  do  fnItems <- catMaybes <$> (genFunDecl `traverse` decls)
      let imports = [ mkUseGlob [ cryptolCrate, "trait_methods"]
                    ]
      pure $ Rust.SourceFile Nothing [] (imports ++ fnItems)

genModule :: GenInfo -> [FunDecl] -> IO (Rust.SourceFile ())
genModule gi ds = runRustM gi (genSourceFile ds)

--------------------------------------------------------------------------------

