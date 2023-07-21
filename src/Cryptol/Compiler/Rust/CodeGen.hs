module Cryptol.Compiler.Rust.CodeGen
  ( genModule
  , ExtModule(..)
  , GenInfo(..)
  )  where

import Language.Rust.Syntax qualified as Rust

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Utils.Ident qualified as Cry
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.CompileType
import Cryptol.Compiler.Rust.CompileTrait
import Cryptol.Compiler.Rust.CompilePrim
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Text(Text)

callPreludePrim :: Text -> Call -> Rust RustExpr
callPreludePrim name call =
  case name of
    "number" -> primNumber call

    -- ("+", [e1, e2]) -> pure $ ring "add" args
    _ -> todo
  where
    args = genExpr `traverse` ircArgs call
    mkTraitCall trait method =
      mkRustCall (pathExpr (simplePath' [trait, method]))
    ring  = mkTraitCall "Ring"
    todo = pure (todoExp ("prelude primitive: " <> Text.unpack name))


-- | Generate Rust code for a Cryptol IR primitive call
callPrim :: IRPrim -> Call -> Rust RustExpr
callPrim p call =
  case p of
    CryPrim (Cry.PrimIdent mod name)
      | mod == Cry.preludeName -> callPreludePrim name call
      | otherwise -> todo


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
      do  -- XXX: adapt arguments if needed.  For example, a function that
          -- works with polymorphic arguments expects a vector.
          -- If we call this function with a known size argument, we need
          -- to turn the array into a vector.

          case ircFun call of
{-
            IRFunVal fnIR ->
              do  fnExpr <- doGenExpr fnIR
                  pure $ mkRustCall fnExpr args'
-}

            IRTopFun tf ->
              do  name <- lookupFunName (irtfName tf)
                  case name of
                    Left prim -> callPrim prim call
                    -- Right nameExpr ->
                    --   args' <- doGenExpr `traverse` ircArgs call
                    --   pure $ mkRustCall nameExpr args'

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
         do tNames    <- mapM (bindLocal addLocalType) (ftTypeParams ft)
            sNames    <- mapM (bindLocal addLocalType . irsName) (ftSizeParams ft)
            aNames    <- mapM (bindLocal addLocalVar) argNames
            argTys    <- mapM compileType (ftParams ft)
            quals     <- mapM compileTrait (ftTraits ft)
            returnTy  <- compileType (ftResult ft)
            funExpr   <- genBlock expr

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
      let imports = [ mkUseGlob ["cryptol","trait_methods"]
                    ]
      pure $ Rust.SourceFile Nothing [] (imports ++ fnItems)

genModule :: GenInfo -> [FunDecl] -> IO (Rust.SourceFile ())
genModule gi ds = runRustM gi (genSourceFile ds)

--------------------------------------------------------------------------------

