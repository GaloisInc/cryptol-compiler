module Cryptol.Compiler.Rust.CodeGen where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import MonadLib

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Data.Position qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Names
-- import Cryptol.Compiler.Rust.Types

type RustType   = Rust.Ty ()
type RustExpr   = Rust.Expr ()
type RustStmt   = Rust.Stmt ()
type RustBlock  = Rust.Block ()

-- convenience

dummySpan :: Rust.Span
dummySpan = Rust.Span Rust.NoPosition Rust.NoPosition

simplePath :: Rust.Ident -> Rust.Path ()
simplePath n = Rust.Path True [Rust.PathSegment n Nothing ()] ()


block :: [RustStmt] -> RustBlock
block stmts = Rust.Block stmts Rust.Normal ()

stmtNoSemi :: RustExpr -> RustStmt
stmtNoSemi e = Rust.NoSemi e ()



-------------------------------------------------------------------------------
-- Generation monad
--
-- TODO

type GenM =
  WithBase Id
    [ ReaderT RO
    , StateT  RW
    ]

newtype Gen a = Gen (GenM a)
  deriving (Functor,Applicative,Monad) via GenM

data RO = RO
  { roModName :: Cry.ModName
    -- ^ The current module we are working on

    -- XXX: The segiments of a mod name are cryptol identifiers
    -- so we'd need to translate those too, although more commonly
    -- these are file names so they are not likely to contain weird
    -- things such as '

  , roExternalNames :: Map Cry.ModName (Map FunName Rust.Ident)
    -- ^ Names defined in different modules. Read only.
  }

data RW = RW
  { rwLocalFunNames :: NameMap FunName
    -- ^ Names in the current module

  , rwLocalNames    :: LocalNames
    -- ^ Names in the current function
  }

-- | Bind a local names
bindLocal :: (a -> LocalNames -> (Rust.Ident,LocalNames)) -> a -> Gen Rust.Ident
bindLocal how x =
  Gen $ sets \rw -> let (i,ls) = how x (rwLocalNames rw)
                    in (i, rw { rwLocalNames = ls })

-- | Bind a function in this module
bindFun :: FunName -> Gen Rust.Ident
bindFun x =
  Gen $ sets \rw -> let (i,fs) = addName x (rwLocalFunNames rw)
                    in (i, rw { rwLocalFunNames = fs })


lookupTParam :: Cry.TParam -> Gen RustType
lookupTParam x =
  Gen
  do i <- lookupName x . lTypeNames . rwLocalNames <$> get
     let seg   = Rust.PathSegment i Nothing ()
         path  = Rust.Path False [seg] ()
     pure (Rust.PathTy Nothing path ())

lookupNameId :: NameId -> Gen RustExpr
lookupNameId x =
  Gen
  do i <- lookupName x . lValNames . rwLocalNames <$> get
     let seg   = Rust.PathSegment i Nothing ()
         path  = Rust.Path False [seg] ()
     pure (Rust.PathExpr [] Nothing path ())





--------------------------------------------------------------------------------

-- | Associate names with Rust identifiers, and keeps track of which
-- rust identifiers we've already used.
data NameMap a = NameMap
  { lUsed :: Set Rust.Ident
  , lMap  :: Map a Rust.Ident
  }

-- | An empty mpa.
emptyNameMap :: Ord a => NameMap a
emptyNameMap = NameMap { lUsed = mempty, lMap  = mempty }

-- | Pick a Rust name for something, ensuring that it does not clash with
-- any previously used names.
addName :: (Ord a, RustIdent a) => a -> NameMap a -> (Rust.Ident, NameMap a)
addName x mp =
  (i, mp { lUsed = Set.insert i (lUsed mp), lMap  = Map.insert x i (lMap mp) })
  where
  used = lUsed mp
  i    = rustIdentAvoiding used (rustIdent x)

lookupName :: (PP a, Ord a) => a -> NameMap a -> Rust.Ident
lookupName x mp =
  case Map.lookup x (lMap mp) of
    Just a  -> a
    Nothing -> panic "lookupName"
                 [ "Undefined name"
                 , show (pp x)
                 ]


-- | Names local to a declaration
data LocalNames = LocalNames
  { lTypeNames  :: NameMap Cry.TParam
  , lValNames   :: NameMap NameId
  }

-- | Empty local names, useful when starting a new declaration.
emptyLocalNames :: LocalNames
emptyLocalNames = LocalNames
  { lTypeNames  = emptyNameMap
  , lValNames   = emptyNameMap
  }

-- | Bind a type parameter.
addLocalType :: Cry.TParam -> LocalNames -> (Rust.Ident, LocalNames)
addLocalType t ns = (i, ns { lTypeNames = mp })
  where
  (i, mp) = addName t (lTypeNames ns)

-- | Bind a local variable/paramter.
addLocalVar :: NameId -> LocalNames -> (Rust.Ident, LocalNames)
addLocalVar x ns = (i, ns { lValNames = mp })
  where
  (i, mp) = addName x (lValNames ns)

--------------------------------------------------------------------------------




{-
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


-}

