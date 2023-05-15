module Cryptol.Compiler.Rust.CodeGen where

import Data.Map (Map)
import Data.Map qualified as Map
import MonadLib

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Language.Rust.Data.Position qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP(pp,cryPP)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.NameMap
import qualified Cryptol.Parser.Lexer as Rust
-- import Cryptol.Compiler.Rust.Types

type RustPath   = Rust.Path ()
type RustType   = Rust.Ty ()
type RustExpr   = Rust.Expr ()
type RustStmt   = Rust.Stmt ()
type RustBlock  = Rust.Block ()
type RustItem   = Rust.Item ()

-- convenience

dummySpan :: Rust.Span
dummySpan = Rust.Span Rust.NoPosition Rust.NoPosition

simplePath :: Rust.Ident -> Rust.Path ()
simplePath n = Rust.Path True [Rust.PathSegment n Nothing ()] ()

block :: [RustStmt] -> RustBlock
block stmts = Rust.Block stmts Rust.Normal ()

stmtNoSemi :: RustExpr -> RustStmt
stmtNoSemi e = Rust.NoSemi e ()

todoExp :: RustExpr
todoExp = Rust.MacExpr [] todoMac ()
  where
  todoMac = Rust.Mac (simplePath "todo") (Rust.Stream []) ()

todoBlock :: RustBlock
todoBlock = block [stmtNoSemi todoExp]

exprStmt :: RustExpr -> RustStmt
exprStmt e = Rust.NoSemi e ()

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

-- | Information about previously compile modules.
data ExtModule = ExtModule
  { extModuleName  :: RustPath
    -- ^ Name of module

  , extModuleNames :: Map FunName Rust.Ident
    -- ^ Functions defined in the module

  -- XXX: When we add support for `newtypes` we should have some type
  -- definitions also.
  }

data RO = RO
  { roModName :: Cry.ModName
    -- ^ The current module we are working on

    -- XXX: The segiments of a mod name are cryptol identifiers
    -- so we'd need to translate those too, although more commonly
    -- these are file names so they are not likely to contain weird
    -- things such as '

  , roExternalNames :: Map Cry.ModName ExtModule
    -- ^ Names defined in different modules. Read only.
  }

data RW = RW
  { rwLocalFunNames :: NameMap FunName
    -- ^ Names in the current module

  , rwLocalNames    :: LocalNames
    -- ^ Names in the current function

  , rwBlockStmt     :: [RustStmt]
    -- ^ Rust statements produced as part of a block.
    --   Note that these are stored in reverse order for performance.
  }

-- | Bind a local names
bindLocal ::
  (a -> LocalNames -> (Rust.Ident,LocalNames)) -> {- ^ How to bind it -}
  a -> {- ^ Name of the local thing -}
  Gen Rust.Ident
bindLocal how x =
  Gen $ sets \rw -> let (i,ls) = how x (rwLocalNames rw)
                    in (i, rw { rwLocalNames = ls })

-- | Bind a function in this module
bindFun :: FunName -> Gen Rust.Ident
bindFun x =
  Gen $ sets \rw -> let (i,fs) = addName x (rwLocalFunNames rw)
                    in (i, rw { rwLocalFunNames = fs })


-- | Get the type corresponding to a type parameter.
lookupTParam :: Cry.TParam -> Gen RustType
lookupTParam x =
  Gen
  do i <- lookupName x . lTypeNames . rwLocalNames <$> get
     let seg   = Rust.PathSegment i Nothing ()
         path  = Rust.Path False [seg] ()
     pure (Rust.PathTy Nothing path ())

-- | Get the expresssion for a local.
lookupNameId :: NameId -> Gen RustExpr
lookupNameId x =
  Gen
  do i <- lookupName x . lValNames . rwLocalNames <$> get
     let seg   = Rust.PathSegment i Nothing ()
         path  = Rust.Path False [seg] ()
     pure (Rust.PathExpr [] Nothing path ())

-- | Get an expression corresponding to a named function
lookupFunName :: FunName -> Gen (Either IRPrim RustExpr)
lookupFunName fu =
  case irfnName fu of
    IRPrimName p -> pure (Left p)
    IRDeclaredFunName f ->
      do let mo = Cry.nameTopModule
                  case f of
                    NameId x -> x
                    AnonId {} ->
                       panic "lookupFunName"
                         [ "Unexpected anonymous function name" ]
         ro <- Gen ask
         rw <- Gen get
         Right . (\x -> Rust.PathExpr [] Nothing x ()) <$>
           if roModName ro == mo
             then
               do let i = lookupName fu (rwLocalFunNames rw)
                      seg = Rust.PathSegment i Nothing ()
                  pure (Rust.Path False [seg] ())
             else
               do ext <- case Map.lookup mo (roExternalNames ro) of
                           Just e -> pure e
                           Nothing ->
                             panic "lookupFunName"
                               [ "Missing module", show (cryPP mo) ]
                  let Rust.Path glob segs _ = extModuleName ext
                  i <- case Map.lookup fu (extModuleNames ext) of
                         Just it -> pure it
                         Nothing -> panic "lookupFunName"
                                      [ "Missing function"
                                      , "Module: " ++ show (cryPP mo)
                                      , "Function: " ++ show (pp fu)
                                      ]
                  let seg = Rust.PathSegment i Nothing ()
                  -- XXX: Maybe we should record that we used this module
                  -- so we can add imports, or maybe they are not needed?
                  pure (Rust.Path glob (segs ++ [seg]) ())


-- | blockScope `c` evaluates a c, returning the list of block
--   statements generated during `c` alongside the value.  This also does
--   not modify any values in the calling computation.
blockScope :: Gen a -> Gen ([RustStmt],a)
blockScope (Gen c) = Gen go
  where
  go =
    do  rw <- sets (\rw -> (rw, rw { rwBlockStmt = []}))
        a <- c
        stmts <- reverse . rwBlockStmt <$> get
        set rw
        pure (stmts, a)

--------------------------------------------------------------------------------
-- Local names

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

nameFromIRName :: IRName t n -> n
nameFromIRName (IRName n _) = n

mkRustCall :: RustExpr -> [RustExpr] -> RustExpr
mkRustCall fn args = Rust.Call [] fn args ()

mkClosure :: [Rust.Ident] -> [RustStmt] -> RustExpr -> RustExpr
mkClosure args stmts expr = Rust.Closure [] move captureBy fnDecl fnExpr ()
  where
    move = Rust.Movable
    captureBy = Rust.Value
    mkArg a = Rust.Arg (Just $ identPat a) (Rust.Infer ()) ()
    args' = mkArg <$> args
    variadic = False
    fnDecl = Rust.FnDecl args' Nothing variadic ()
    fnExpr = blockExprIfNeeded stmts expr


blockExprIfNeeded :: [RustStmt] -> RustExpr -> RustExpr
blockExprIfNeeded stmts e =
  case stmts of
    [] -> e
    _ -> Rust.BlockExpr [] (block (stmts ++ [Rust.NoSemi e ()])) ()

identPat :: Rust.Ident -> Rust.Pat ()
identPat ident = Rust.IdentP bindingMode ident Nothing ()
  where
    bindingMode = Rust.ByValue Rust.Immutable


emitBlockStmt :: RustStmt -> Gen ()
emitBlockStmt s =
  Gen $ sets_ (\rw -> rw { rwBlockStmt = s:rwBlockStmt rw})


callPrim :: IRPrim -> [RustExpr] -> Gen RustExpr
callPrim = undefined

genExpr :: Expr -> Gen RustExpr
genExpr (IRExpr e0) =
  case e0 of
    IRVar (IRName name _) -> lookupNameId name
    IRCallFun call ->
      do  args' <- genExpr `traverse` ircArgs call
          case ircFun call of
            IRFunVal fnIR ->
              do  fnExpr <- genExpr fnIR
                  pure $ mkRustCall fnExpr args'

            IRTopFun tf ->
              do  name <- lookupFunName (irtfName tf)
                  case name of
                    Left prim -> callPrim prim args'
                    Right nameExpr -> pure $ mkRustCall nameExpr args'

    IRClosure call -> undefined

    IRLam args expr ->
      do  let args' = nameFromIRName <$> args
          args'' <- bindLocal addLocalVar `traverse` args'
          (lamStmt, lamE) <- blockScope (genExpr expr)
          pure $ mkClosure args'' lamStmt lamE


    IRIf eTest eThen eElse ->
      do  eTest' <- genExpr eTest
          (eThenStmts, eThen') <- blockScope $ genExpr eThen
          (eElseStmts, eElse') <- blockScope $ genExpr eElse
          let thenBlock = block (eThenStmts ++ [exprStmt eThen'])
              elseBlock = block (eElseStmts ++ [exprStmt eElse'])
          let elseBlockExpr =
                case eElseStmts of
                  [] -> eElse'
                  _stmts -> Rust.BlockExpr [] elseBlock ()

              eif = Rust.If [] eTest' thenBlock (Just elseBlockExpr) ()

          pure eif

    IRLet (IRName name _) eBound eIn ->
      do  eBound' <- genExpr eBound
          boundIdent <- bindLocal addLocalVar name
          let -- TODO add explicit type?
              ty = Nothing
              letBind = Rust.Local (identPat boundIdent) ty (Just eBound') [] ()

          emitBlockStmt letBind
          genExpr eIn


