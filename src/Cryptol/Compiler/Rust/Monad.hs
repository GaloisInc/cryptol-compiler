module Cryptol.Compiler.Rust.Monad where

import Data.Map (Map)
import Data.Map qualified as Map
import MonadLib

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP(pp,cryPP)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.NameMap

data GenInfo =
  GenInfo
    { genCurModule :: Cry.ModName
    , genExternalModules :: Map Cry.ModName ExtModule
    }

runRustM :: GenInfo -> Rust a -> a
runRustM gi (Rust m) = fst $ runId $ runStateT rw $ runReaderT ro m
  where
  ro =
    RO
      { roModName = genCurModule gi
      , roExternalNames = genExternalModules gi
      }
  rw =
    RW
      { rwLocalFunNames = emptyNameMap
      , rwLocalNames    = emptyLocalNames
      }


type RustImpl =
  WithBase Id
    [ ReaderT RO
    , StateT  RW
    ]

newtype Rust a = Rust (RustImpl a)
  deriving (Functor,Applicative,Monad) via RustImpl

-- | Information about previously compiled modules.
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

    -- XXX: The segments of a mod name are cryptol identifiers
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
  }



-- | Bind a local names
bindLocal ::
  (a -> LocalNames -> (Rust.Ident,LocalNames)) -> {- ^ How to bind it -}
  a -> {- ^ Name of the local thing -}
  Rust Rust.Ident
bindLocal how x =
  Rust $ sets \rw -> let (i,ls) = how x (rwLocalNames rw)
                    in (i, rw { rwLocalNames = ls })

-- | Bind a function in this module
bindFun :: FunName -> Rust Rust.Ident
bindFun x =
  Rust $ sets \rw -> let (i,fs) = addName x (rwLocalFunNames rw)
                    in (i, rw { rwLocalFunNames = fs })


getTParams :: Rust (Cry.TParam -> RustType)
getTParams =
  do tys <- lTypeNames . rwLocalNames <$> Rust get
     pure \x ->
       let i = lookupName x tys
           seg   = Rust.PathSegment i Nothing ()
           path  = Rust.Path False [seg] ()
       in Rust.PathTy Nothing path ()

-- | Get the type corresponding to a type parameter.
lookupTParam :: Cry.TParam -> Rust RustType
lookupTParam x =
  Rust
  do i <- lookupName x . lTypeNames . rwLocalNames <$> get
     let seg   = Rust.PathSegment i Nothing ()
         path  = Rust.Path False [seg] ()
     pure (Rust.PathTy Nothing path ())

-- | Get the expresssion for a local.
lookupNameId :: NameId -> Rust RustExpr
lookupNameId x =
  Rust
  do i <- lookupName x . lValNames . rwLocalNames <$> get
     let seg   = Rust.PathSegment i Nothing ()
         path  = Rust.Path False [seg] ()
     pure (Rust.PathExpr [] Nothing path ())

-- | Get an expression corresponding to a named function
lookupFunName :: FunName -> Rust (Either IRPrim RustExpr)
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
         ro <- Rust ask
         rw <- Rust get
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

-- | Evaluate a computation, forgetting any locally bound variables afterward.
localScope :: Rust a -> Rust a
localScope (Rust ma) =
  Rust
  do  names <- rwLocalNames <$> get
      r <- ma
      _ <- sets_ (\s -> s { rwLocalNames = names })
      pure r

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



