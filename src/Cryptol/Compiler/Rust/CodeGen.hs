module Cryptol.Compiler.Rust.CodeGen
  ( genModule
  , ExtModule(..)
  , GenInfo(..)
  )  where

import Language.Rust.Syntax qualified as Rust

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.Types (rustRep)
import Data.Maybe (catMaybes)

-- | Generate Rust code for a Cryptol IR primitive call
callPrim :: IRPrim -> [RustExpr] -> Rust RustExpr
callPrim p es =
  case p of
    CryPrim name  -> todo
    MakeSeq       -> todo
    Tuple         -> todo
    TupleSel n _  -> todo
    Map           -> todo
    FlatMap       -> todo
    Zip           -> todo

    Collect       -> todo
    Iter          -> todo
  where
  todo = pure (todoExp (show (pp p)))
  primE =  undefined

-- | Generate Rust code for a Cryptol IR expression, creating a block if
doGenExpr :: Expr -> Rust RustExpr
doGenExpr e =
  do (stmts,expr) <- genExpr e
     pure (blockExprIfNeeded stmts expr)

-- | Generate a Rust block corresponding to a Cryptol IR expression
genBlock :: Expr -> Rust RustBlock
genBlock e = uncurry block' <$> genExpr e

-- | From a Cryptol IR expression, generate a Rust expressions along with
--   a set of Rust statements that contextualize it, such as let bindings
genExpr :: Expr -> Rust ([RustStmt], RustExpr)
genExpr (IRExpr e0) =
  let justExpr e = ([],e)
  in
  case e0 of
    IRVar (IRName name _) -> justExpr <$> lookupNameId name

    IRCallFun call ->
      justExpr <$>
      do  args' <- doGenExpr `traverse` ircArgs call
          -- XXX: adapt arguments if needed.  For example, a function that
          -- works with polymorphic arguments expects a vector.
          -- If we call this function with a known size argument, we need
          -- to turn the array into a vector.

          case ircFun call of
            IRFunVal fnIR ->
              do  fnExpr <- doGenExpr fnIR
                  pure $ mkRustCall fnExpr args'

            IRTopFun tf ->
              do  name <- lookupFunName (irtfName tf)
                  case name of
                    Left prim -> callPrim prim args'
                    Right nameExpr -> pure $ mkRustCall nameExpr args'

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

-- | Build the associated Rust type for the IR type
rustTy :: Type -> Rust RustType
rustTy ty =
  do tys <- getTParams
     let ?poly = tys
     pure (rustRep ty)

genTrait :: Trait -> Rust RustWherePredicate
genTrait (IRTrait name arg) =
  do tparam <- getTParams
     pure (Rust.BoundPredicate [] (tparam arg) [bound] ())
  where
  bound     = Rust.TraitTyParamBound traitName Rust.None ()
  traitName = Rust.PolyTraitRef [] (Rust.TraitRef path) ()
  -- XXX: qualify these?
  path =
    simplePath
      case name of
        PZero         -> "Zero"
        PLogic        -> "Logic"
        PRing         -> "Ring"
        PIntegral     -> "Integral"
        PField        -> "Field"
        PRound        -> "Round"

        PEq           -> "Eq" -- Reuse Rust's?
        PCmp          -> "Cmp"
        PSignedCmp    -> "SignedCmp"

        PLiteral      -> "Literal"
        PFLiteral     -> "FLiteral"



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
    do name <- bindFun (irfName decl)
       let ft = irfType decl

       localScope
         do tNames <- mapM (bindLocal addLocalType) (ftTypeParams ft)
            aNames <- mapM (bindLocal addLocalVar) argNames
            argTys <- mapM rustTy (ftParams ft)
            quals  <- mapM genTrait (ftTraits ft)
            returnTy <- rustTy (ftResult ft)
            funExpr <- genBlock expr

            let params = zip aNames argTys
            let tyParams = [ Rust.TyParam [] i [] Nothing () | i <- tNames ]
                lifetimes   = []
                whereClause = Rust.WhereClause quals ()
                generics    = mkGenerics lifetimes tyParams whereClause
            pure (Just (mkFnItem name generics params returnTy funExpr))


-- | Given a set of FunDecls, make a Rust SourceFile
genSourceFile :: [FunDecl] -> Rust (Rust.SourceFile ())
genSourceFile decls =
  do  fnItems <- catMaybes <$> (genFunDecl `traverse` decls)
      pure $ Rust.SourceFile Nothing [] fnItems

genModule :: GenInfo -> [FunDecl] -> Rust.SourceFile ()
genModule gi ds = runRustM gi (genSourceFile ds)


