module Cryptol.Compiler.Rust.CodeGen
  ( genModule
  , ExtModule(..)
  , GenInfo(..)
  )  where

import Language.Rust.Syntax qualified as Rust

import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Types (rustRep)
import Data.Maybe (catMaybes)

-- | Generate Rust code for a Cryptol IR primitive call
callPrim :: IRPrim -> [RustExpr] -> Gen RustExpr
callPrim _ _ = pure todoExp

-- | Generate Rust code for a Cryptol IR expression, creating a block if
doGenExpr :: Expr -> Gen RustExpr
doGenExpr e =
  do (stmts,expr) <- genExpr e
     pure (blockExprIfNeeded stmts expr)

-- | Generate a Rust block corresponding to a Cryptol IR expression
genBlock :: Expr -> Gen RustBlock
genBlock e = uncurry block' <$> genExpr e

-- | From a Cryptol IR expression, generate a Rust expressions along with
--   a set of Rust statements that contextualize it, such as let bindings
genExpr :: Expr -> Gen ([RustStmt], RustExpr)
genExpr (IRExpr e0) =
  let justExpr e = ([],e)
  in
  case e0 of
    IRVar (IRName name _) -> justExpr <$> lookupNameId name

    IRCallFun call ->
      justExpr <$>
      do  args' <- doGenExpr `traverse` ircArgs call
          case ircFun call of
            IRFunVal fnIR ->
              do  fnExpr <- doGenExpr fnIR
                  pure $ mkRustCall fnExpr args'

            IRTopFun tf ->
              do  name <- lookupFunName (irtfName tf)
                  case name of
                    Left prim -> callPrim prim args'
                    Right nameExpr -> pure $ mkRustCall nameExpr args'

    IRClosure call -> pure (justExpr todoExp)

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

-- | Build the associated Rust type for the IR type
rustTy :: Type -> Gen RustType
rustTy ty =
  do tys <- getTParams
     let ?poly = tys
     pure (rustRep ty)

-- | Generate a RustItem corresponding to a function declaration.
--   Returns `Nothing` if the declaration is for an IR primitive.
genFunDecl :: FunDecl -> Gen (Maybe RustItem)
genFunDecl decl =
  case irfDef decl of
    -- TODO: maybe this would be a good place to check that the set
    --       of implemented primitives is complete?
    IRFunPrim -> pure Nothing
    IRFunDef argNames expr ->
      do name <- bindFun (irfName decl)
         let ft       = irfType decl
             traits   = ftTraits ft     -- XXX

         localScope
           do tNames <- mapM (bindLocal addLocalType) (ftTypeParams ft)
              aNames <- mapM (bindLocal addLocalVar) argNames
              argTys <- mapM rustTy (ftParams ft)
              returnTy <- rustTy (ftResult ft)
              funExpr <- genBlock expr

              let params = zip aNames argTys
              let tyParams    =[ Rust.TyParam [] i [] Nothing () | i <- tNames ]
                  lifetimes   = []
                  whereClause = Rust.WhereClause [] () -- XXX: from traits
                  generics    = mkGenerics lifetimes tyParams whereClause
              pure (Just (mkFnItem name generics params returnTy funExpr))


-- | Given a set of FunDecls, make a Rust SourceFile
genSourceFile :: [FunDecl] -> Gen (Rust.SourceFile ())
genSourceFile decls =
  do  fnItems <- catMaybes <$> (genFunDecl `traverse` decls)
      pure $ Rust.SourceFile Nothing [] fnItems

genModule :: GenInfo -> [FunDecl] -> Rust.SourceFile ()
genModule gi ds = runGenM gi (genSourceFile ds)
