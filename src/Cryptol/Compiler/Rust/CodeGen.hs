module Cryptol.Compiler.Rust.CodeGen
  ( genModule
  , ExtModule(..)
  , GenInfo(..)
  )  where

import Data.Set qualified as Set
import Language.Rust.Syntax qualified as Rust
import Control.Monad(forM, mapAndUnzipM)

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Utils.Ident qualified as Cry
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Monad
import Cryptol.Compiler.Rust.CompileSize
import Cryptol.Compiler.Rust.CompileType
import Cryptol.Compiler.Rust.CompileTrait
import Cryptol.Compiler.Rust.CompilePrim
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Text(Text)
import Cryptol.Compiler.Monad (panic)

-- callPreludePrim :: Text -> Call -> Rust ([RustStmt], RustExpr)
-- callPreludePrim name call =
--   case name of
--     -- Literal --
--     "number" -> primNumber call

--     -- Ring --
--     "+" -> ring "add"
--     "-" -> ring "sub"
--     "*" -> ring "mul"
--     "negate" -> ring "negate"
--     "fromInteger" -> ring "from_integer"
--     -- TODO: do we need to figure out if the exponent will fit in
--     --       a u32 before calling this? (or call `Integral::to_usize`?)
--     "^^" -> todo

--     -- Integral --
--     "/" -> integral "div"
--     "%" -> integral "modulo"
--     "toInteger" -> integral "to_integer"

--     -- Zero
--     "zero" -> callTrait "Zero" "zero"

--     -- Logic
--     "&&" -> logic "and"
--     "||" -> logic "or"
--     "^" -> logic "xor"
--     "complement" -> logic "complement"

--     _ -> todo
--   where
--     callTrait trait fnName =
--       withEvalArgs' $ \es ->
--         justExpr (mkTraitCall trait fnName es)

--     ring = callTrait "Ring"
--     integral = callTrait "Integral"
--     logic = callTrait "Logic"

--     args = ircArgs call
--     withEvalArgs' f = withEvalArgs (pure . f)
--     withEvalArgs f =
--       do  (stmts, exps) <- unzip <$> genExpr `traverse` args
--           (fstmts, fexp) <- f exps
--           pure (concat stmts ++ fstmts, fexp)

--     mkTraitCall trait method =
--       mkRustCall (pathExpr (simplePath' [trait, method]))
--     todo =
--       pure $ justExpr (todoExp ("prelude primitive: " <> Text.unpack name))


getPreludePrimFnExpr :: Text -> Maybe RustExpr
getPreludePrimFnExpr name =
  case name of
      -- Ring --
    "+" -> ring "add"
    "-" -> ring "sub"
    "*" -> ring "mul"
    "negate" -> ring "negate"
    "fromInteger" -> ring "from_integer"
    -- TODO: do we need to figure out if the exponent will fit in
    --       a u32 before calling this? (or call `Integral::to_usize`?)
    "^^" -> Nothing

    -- Integral --
    "/" -> integral "div"
    "%" -> integral "modulo"
    "toInteger" -> integral "to_integer"

    -- Zero
    "zero" -> Nothing -- TODO

    -- Logic
    "&&" -> logic "and"
    "||" -> logic "or"
    "^" -> logic "xor"
    "complement" -> logic "complement"
    _ -> Nothing
  where
    ring = mkTraitFnExpr "Ring"
    integral = mkTraitFnExpr "Integral"
    logic = mkTraitFnExpr "Logic"



    mkTraitFnExpr trait method =
      Just (pathExpr (simplePath' [trait, method]))

getPrimFnExpr :: IRPrim -> Maybe RustExpr
getPrimFnExpr prim =
  case prim of
    CryPrim (Cry.PrimIdent mod name)
      | mod == Cry.preludeName -> getPreludePrimFnExpr name
      | otherwise -> Nothing
    _ -> Nothing


-- -- | Generate Rust code for a Cryptol IR primitive call
-- callPrim :: IRPrim -> Call -> Rust ([RustStmt], RustExpr)
-- callPrim p call =
--   case p of
--     CryPrim (Cry.PrimIdent mod name)
--       | mod == Cry.preludeName -> callPreludePrim name call
--       | otherwise -> todo


--     MakeSeq       -> todo
--     Tuple         -> todo
--     TupleSel n _  -> todo
--     Map           -> todo
--     FlatMap       -> todo
--     Zip           -> todo

--     Collect       -> todo
--     Iter          -> todo
--   where
--   todo = pure $ justExpr (todoExp (show (pp p)))
--   primE =  undefined

-- | Generate Rust code for a Cryptol IR expression, creating a block if
doGenExpr :: Expr -> Rust RustExpr
doGenExpr e =
  do (stmts,expr) <- genExpr e
     pure (blockExprIfNeeded stmts expr)

-- | Generate a Rust block corresponding to a Cryptol IR expression
genBlock :: Expr -> Rust RustBlock
genBlock e = uncurry block' <$> genExpr e




genCall :: Call -> Rust ([RustStmt], RustExpr)
genCall call =
  do  argExprs <- traverse doGenExpr (ircArgs call)
      let funTy = ircFunType call
          needLen = Set.unions (map traitNeedsLen (ftTraits funTy))
          typeArgs =
            case ircFun call of
              IRFunVal _ -> []
              IRTopFun tf -> irtfTypeArgs tf

          lenParams = [ arg | (t, arg) <- ftTypeParams funTy `zip` typeArgs
                            , t `Set.member` needLen ]

      lenParamExprs <- traverse lenParamFor lenParams

      szExprs <-
        case ircFun call of
          IRFunVal _ -> pure []
          IRTopFun tf -> traverse (uncurry compileSize) (irtfSizeArgs tf)

      fnExpr <-
        case ircFun call of
          IRFunVal fnIR -> doGenExpr fnIR
          IRTopFun tf ->
            do  name <- lookupFunName (irtfName tf)
                case name of
                  Left prim ->
                    case getPrimFnExpr prim of
                      Nothing -> panic "genCall" ["primitive not implemented"]
                      Just p -> pure p
                  Right path ->
                    do  tys' <- traverse compileType (irtfTypeArgs tf)
                        pure $ pathExpr (pathAddTypeSuffix path tys')

      pure $ justExpr (mkRustCall fnExpr (lenParamExprs ++ szExprs ++ argExprs))

-- | From a Cryptol IR expression, generate a Rust expressions along with
--   a set of Rust statements that contextualize it, such as let bindings
genExpr :: Expr -> Rust ([RustStmt], RustExpr)
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

