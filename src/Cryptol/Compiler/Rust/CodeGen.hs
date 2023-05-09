module Cryptol.Compiler.Rust.CodeGen where

import Data.Functor.Identity (Identity)
import qualified Data.Char as Char
import qualified Cryptol.TypeCheck.AST as Cry
import qualified MonadLib as Monad
import Data.Map (Map)

import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Data.Ident as RustIdent
import qualified Language.Rust.Data.Position as RustPos

import Cryptol.Compiler.IR.Cryptol qualified as IR
-- import Cryptol.Compiler.Rust.Types

type RustExpr = Rust.Expr ()
type RustStmt = Rust.Stmt ()
type RustBlock = Rust.Block ()

-- convenience

dummySpan :: RustPos.Span
dummySpan = RustPos.Span RustPos.NoPosition RustPos.NoPosition

simplePath :: RustIdent.Ident -> Rust.Path ()
simplePath n = Rust.Path True [Rust.PathSegment n Nothing ()] ()


block :: [RustStmt] -> RustBlock
block stmts = Rust.Block stmts Rust.Normal ()

stmtNoSemi :: RustExpr -> RustStmt
stmtNoSemi e = Rust.NoSemi e ()



-------------------------------------------------------------------------------
-- Generation monad
--
-- TODO

type Gen a = Monad.StateT GenerationState Monad.Id a
data GenerationState = GenerationState
  { gsNameMapping :: Map Cry.Name RustIdent.Ident
  }


genFunDecl :: IR.FunDecl -> Gen  ()
genFunDecl fndecl =
  case IR.irfnName $ IR.irfName fndecl of
    IR.IRPrimName pname -> error ("genFunDecl: Not expecting to see fun decl for " ++ show pname)
    -- TODO: check that these are already in the environment
    -- IR.IRCryPrimName primIdent -> pure ()
    IR.IRDeclaredFunName dfn -> undefined

newDeclName :: Cry.Name -> Gen RustIdent.Ident
newDeclName = undefined

addItem :: Rust.Item () -> Gen ()
addItem = undefined


genDeclaredFun :: Cry.Name -> IR.FunType -> IR.FunDef -> Gen ()
genDeclaredFun name ty def =
  do  name' <- newDeclName name
      let fn = Rust.Fn [] visibility name' decl safety isConst abi generics impl ()
      addItem fn
  where
    safety = Rust.Normal
    isConst = Rust.Const
    abi = Rust.RustCall
    visibility = Rust.PublicV
    variadic = False
    args = []
    retTy = Nothing
    decl = Rust.FnDecl args retTy variadic ()
    generics = Rust.Generics [] [] (Rust.WhereClause [] ()) () -- TODO
    impl = todoBlock -- TODO


-- Insert an empty `todo!`
todoBlock :: RustBlock
todoBlock = block [stmtNoSemi macExpr]
  where
    macExpr = Rust.MacExpr [] todoMac ()
    todoMac = Rust.Mac (simplePath "todo") (Rust.Stream []) ()


type BlockGen a = Identity a

blockStmt :: RustStmt -> BlockGen ()
blockStmt s = undefined

runGenBlock :: BlockGen a -> Gen a
runGenBlock bg = undefined

liftGen :: Gen a -> BlockGen a
liftGen g = undefined

subBlock :: IR.IRExpr tn n -> BlockGen RustBlock
subBlock b = undefined

blockMkName :: IR.Name -> BlockGen RustIdent.Ident
blockMkName = undefined

blockGetName :: IR.Name  -> BlockGen RustIdent.Ident
blockGetName = undefined

blockVar :: IR.Name -> BlockGen RustExpr
blockVar n = undefined

blockBindVar ::  IR.Name -> RustExpr -> BlockGen ()
blockBindVar n e = undefined

simpleVarExpr :: RustIdent.Ident -> Rust.Expr ()
simpleVarExpr n = Rust.PathExpr [] Nothing (varPath n) ()
  where
    varPath n = Rust.Path True [Rust.PathSegment n Nothing ()] ()

letBind :: IR.Name -> IR.Expr -> BlockGen ()
letBind name expr =
  do  expr' <- genExpr expr
      name' <- blockMkName name
      blockStmt (letStmt name' expr')
      blockBindVar name (simpleVarExpr name')

  where
    letStmt n e = Rust.Local (pat n) Nothing (Just e) [] ()
    pat n = Rust.IdentP (Rust.ByValue Rust.Immutable) n Nothing ()


genExpr :: IR.Expr -> BlockGen RustExpr
genExpr (IR.IRExpr e0) =
  case e0 of
    IR.IRVar name -> blockVar name
    IR.IRCallFun call ->
      do  argExprs <- genExpr `traverse` IR.ircArgs call
          undefined


    IR.IRClosure call -> undefined
    IR.IRLam names expr -> undefined
    IR.IRIf testExpr thenExpr elseExpr ->
      do  testExpr' <- genExpr testExpr
          thenBlock <- subBlock thenExpr
          elseExpr' <- genExpr elseExpr

          pure $ Rust.If [] testExpr' thenBlock (Just elseExpr') ()
{-
    IR.IRTuple es ->
      do  es' <- genExpr `traverse` es
          pure $ Rust.TupExpr [] es' ()
-}
    IR.IRLet name boundExpr inExpr ->
      letBind name boundExpr >> genExpr inExpr





-- funDecl :: IR.IRFunDecl -> Gen (Maybe Rust.FnDecl)
-- funDecl decl =
--   case IR.irfDef decl of
--     IR.IRFunPrim -> pure Nothing
--     IR.IRFunDef _ (IR.IRExpr expr) ->



-------------------------------------------------------------------------------

asRustVarName :: String -> String
asRustVarName s = s >>= rustify
  where
    rustify c = if Char.isUpper c then ['_', Char.toLower c] else [c]


