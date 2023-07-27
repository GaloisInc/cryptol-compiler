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
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.CompileSize
import Cryptol.Compiler.Rust.CompileType
import Cryptol.Compiler.Rust.CompileTrait
import Cryptol.Compiler.Rust.CompilePrim

-- | Generate Rust code for a Cryptol IR expression, creating a block if
doGenExpr :: Expr -> Rust RustExpr
doGenExpr e =
  do (stmts,expr) <- genExpr e
     pure (blockExprIfNeeded stmts expr)

-- | Generate a Rust block corresponding to a Cryptol IR expression
genBlock :: Expr -> Rust RustBlock
genBlock e = uncurry block' <$> genExpr e


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
-- XXX: we probably want to pass references
genCallArgs :: Call -> Rust ([RustStmt], [RustExpr])
genCallArgs call =
  do (stmtss,es) <- unzip <$> traverse genExpr (ircArgs call)
     pure (concat stmtss, es)

genCall :: Call -> Rust (PartialBlock RustExpr)
genCall call =

  case ircFun call of

    -- Closures don't have polymorphic arguments, and so shouldn't have
    -- size or lenght arguments. Captured length arguments should be in scope.
    IRFunVal fnIR ->
      do fnExpr       <- doGenExpr fnIR
         (stmts,args) <- genCallArgs call
         pure (stmts, mkRustCall fnExpr args)

    IRTopFun tf ->
      do typeArgs     <- traverse compileType (irtfTypeArgs tf)
         lenArgs      <- genCallLenArgs call
         szArgs       <- genCallSizeArgs call
         (stmts,args) <- genCallArgs call
         name         <- lookupFunName (irtfName tf)
         val <- case name of
                 Left prim -> compilePrim prim
                              PrimArgs
                                { primTypeArgs = typeArgs
                                , primLenArgs  = lenArgs
                                , primSizeArgs = szArgs
                                , primArgs     = args
                                }
                 Right path ->
                   do let fun = pathExpr (pathAddTypeSuffix path typeArgs)
                          allArgs = lenArgs ++ szArgs ++ args
                      pure (mkRustCall fun allArgs)
         pure (stmts,val)


-- | From a Cryptol IR expression, generate a Rust expressions along with
--   a set of Rust statements that contextualize it, such as let bindings
genExpr :: Expr -> Rust (PartialBlock RustExpr)
genExpr (IRExpr e0) =
  case e0 of
    IRVar (IRName name _) -> justExpr <$> lookupNameId name

    IRCallFun call -> genCall call

    IRClosure call -> pure (justExpr (todoExp (show (pp call))))

    IRLam args expr ->
      justExpr <$>
      do  let args' = irNameName <$> args
          args'' <- bindLocal addLocalVar `traverse` args'
          (lamStmt, lamE) <- genExpr expr
          pure $ mkClosure args'' lamStmt lamE


    IRIf eTest eThen eElse ->
      justExpr <$>
      do  eTest' <- doGenExpr eTest
          thenBlock <- genBlock eThen
          elseBlockExpr <- doGenExpr eElse
          pure $ Rust.If [] eTest' thenBlock (Just elseBlockExpr) ()

    IRLet (IRName name _) eBound eIn ->
      do  eBound'    <- doGenExpr eBound
          boundIdent <- bindLocal addLocalVar name
          let -- TODO add explicit type?
              ty = Nothing
              letBind = Rust.Local (identPat boundIdent) ty (Just eBound') [] ()
          (stms,e) <- genExpr eIn
          pure (letBind:stms,e)


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
            -- XXX: We probably want to use references
            nParams <- forM (argNames `zip` ftParams ft) \(arg,ty) ->
                        do i <- bindLocal addLocalVar arg
                           t <- compileType ty
                           pure (i,t)

            returnTy  <- compileType (ftResult ft)
            funExpr   <- genBlock expr

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
      let imports = [ mkUseGlob ["cryptol","trait_methods"]
                    ]
      pure $ Rust.SourceFile Nothing [] (imports ++ fnItems)

genModule :: GenInfo -> [FunDecl] -> IO (Rust.SourceFile ())
genModule gi ds = runRustM gi (genSourceFile ds)

--------------------------------------------------------------------------------

