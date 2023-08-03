module Cryptol.Compiler.Rust.Monad where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MonadLib

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP(pp,cryPP)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.NameMap

data GenInfo =
  GenInfo
    { genCurModule :: Cry.ModName
    , genExternalModules :: Map Cry.ModName ExtModule
    }

runRustM :: GenInfo -> Rust a -> IO a
runRustM gi (Rust m) = fst <$> runStateT rw (runReaderT ro m)
  where
  ro =
    RO
      { roModName = genCurModule gi
      , roExternalNames = genExternalModules gi
      }
  rw =
    RW
      { rwLocalFunNames   = emptyNameMap
      , rwLocalNames      = emptyLocalNames
      , rwLocalVars       = Set.empty
      }


type RustImpl =
  WithBase IO
    [ ReaderT RO
    , StateT  RW
    ]

newtype Rust a = Rust (RustImpl a)
  deriving (Functor,Applicative,Monad) via RustImpl

-- | Run an IO computation in the Rust monad
doIO :: IO a -> Rust a
doIO io = Rust (inBase io)

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

  , rwLocalVars     :: Set Rust.Ident
    -- ^ Names of local variable (i.e., not arguments)

  , rwPolyArrayParams :: Map Size Rust.Ident
    -- ^ A Rust type varaible to use for sequences of the given length.
    -- This is only use for sequences that are not of a fixed known size.
  }


-- | Reset names local to a function.
resetFunLocalNames :: Rust ()
resetFunLocalNames = Rust (sets_ reset)
  where
  reset rw = rw { rwLocalNames = emptyLocalNames
                , rwLocalVars  = Set.empty
                , rwPolyArrayParams = Map.empty
                }
-- | Bind a local names
bindLocal ::
  Bool {- ^ Is this a local variable name (i.e., not parameter -} ->
  (a -> LocalNames -> (Rust.Ident,LocalNames)) {- ^ How to bind it -} ->
  a {- ^ Name of the local thing -} ->
  Rust Rust.Ident
bindLocal isLocal how x =
  Rust $ sets \rw -> let (i,ls) = how x (rwLocalNames rw)
                         rw1 = if isLocal
                                  then rw { rwLocalVars = Set.insert i
                                                              (rwLocalVars rw) }
                                  else rw
                    in (i, rw1 { rwLocalNames = ls })

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
  Rust (pathType . simplePath . lookupName x . lTypeNames . rwLocalNames <$> get)

-- | Get the expresssion for a size parameter.
lookupSizeParam :: Cry.TParam -> Rust RustExpr
lookupSizeParam x =
  Rust (pathExpr . simplePath . lookupName x . lTypeNames . rwLocalNames <$> get)

-- | Get the expresssion for a local.  It also return `True` if this is
-- a local variable, as opposed to an argument.
lookupNameId :: NameId -> Rust (Bool,RustExpr)
lookupNameId x =
  do rw <- Rust get
     let rid = lookupName x (lValNames (rwLocalNames rw))
     let loc = rid `Set.member` rwLocalVars rw
     pure (loc, pathExpr (simplePath rid))


-- | Get the expression for a length parametes associated with the given
-- type parameter.
lookupLenParam :: Cry.TParam -> Rust RustExpr
lookupLenParam x =
  Rust (pathExpr . simplePath . doLookup . lLenParams . rwLocalNames <$> get)
  where
  doLookup mp =
    case Map.lookup x mp of
      Just a  -> a
      Nothing -> panic "lookupLenParam"
                   [ "Undefined length parameter."
                   , show (pp x)
                   ]

-- | Is this name in the module that we are currently compiling.
isFunNameLocal :: FunName -> Rust Bool
isFunNameLocal fu =
  do cur <- roModName <$> Rust ask
     pure case irfnName fu of
            IRDeclaredFunName name -> nameIdModule name == cur
            _ -> False

-- | Get an expression corresponding to a named function
lookupFunName :: FunName -> Rust (Either IRPrim RustPath)
lookupFunName fu =
  case irfnName fu of
    IRPrimName p -> pure (Left p)
    IRDeclaredFunName f ->
      do let mo = nameIdModule f
         ro <- Rust ask
         rw <- Rust get
         Right <$>
           if roModName ro == mo
             then
               do let i = lookupName fu (rwLocalFunNames rw)
                      seg = Rust.PathSegment i Nothing ()
                  pure (Rust.Path False [seg] ())
             else
               do ext <- case Map.lookup mo (roExternalNames ro) of
                           Just e -> pure e
                           Nothing ->
                             panic "lookupFunName" $
                               [ "Function that failed to resolve"
                               , show (pp fu)
                               , "Missing module"
                               , show (cryPP mo)
                               , "External modules"
                               ] ++ map (show.cryPP)
                                   (Map.keys (roExternalNames ro))
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
  { lTypeNames  :: NameMap Cry.TParam           -- ^ Names for type params
  , lValNames   :: NameMap NameId               -- ^ Names for local values
  , lLenParams  :: Map Cry.TParam Rust.Ident
    -- ^ Names for Length params.  When generating these we use the "used"
    -- set of `lValNames`, because they are values.
  }

-- | Empty local names, useful when starting a new declaration.
emptyLocalNames :: LocalNames
emptyLocalNames = LocalNames
  { lTypeNames  = emptyNameMap
  , lValNames   = emptyNameMap
  , lLenParams  = mempty
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

-- | Bind a local parameter used for the Length trait.
addLocalLenghtParam :: Cry.TParam -> LocalNames -> (Rust.Ident, LocalNames)
addLocalLenghtParam t ns = (i, ns { lValNames  = newVals
                                  , lLenParams = newPa
                                  })
  where
  vals    = lValNames ns
  used    = lUsed vals
  i       = rustIdentAvoiding used (rustIdent (TraitLengthName t))
  newVals = vals { lUsed = Set.insert i used }
  newPa   = Map.insert t i (lLenParams ns)







--------------------------------------------------------------------------------



