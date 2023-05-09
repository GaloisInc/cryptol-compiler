-- | The compiler monad, used to keep track of state, configuration, errors,
-- etc.
module Cryptol.Compiler.Monad
  (
    -- * Compiler Monad
    CryC, runCryC

    -- * Loaded Modules
  , loadModuleByPath
  , getLoadedModules

    -- * Names of Primitives
  , getPrimTypeName
  , getPrimDeclName
  , isPrimDecl
  , isPrimType
  , Cry.preludeName
  , Cry.floatName

    -- * Errors and Warnings
  , addWarning
  , throwError
  , catchError
  , unsupported
  , panic
  , catchablePanic

    -- * IO
  , doIO

    -- * Locals
  , withCryLocals

    -- * Types
  , getTypeOf
  , getSchemaOf
  , getTopTypes
  , getFun

    -- * Names generation
  , newNameId

    -- * IR generation
  , addCompiled
  , getCompiled

    -- * SMT solver
  , getSolver
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Control.Exception
import MonadLib
import MonadLib qualified as MLib
import qualified Data.ByteString as BS
import Data.Map(Map)
import qualified Data.Map as Map

import qualified Cryptol.ModuleSystem as Cry
import qualified Cryptol.ModuleSystem.Env as Cry
import qualified Cryptol.TypeCheck.InferTypes as Cry
import qualified Cryptol.TypeCheck.Solver.SMT as Cry
import qualified Cryptol.TypeCheck.AST as Cry
import qualified Cryptol.TypeCheck.TypeOf as Cry
import qualified Cryptol.Utils.Ident as Cry
import qualified Cryptol.Utils.PP as Cry
import qualified Cryptol.Utils.Logger as Cry
import qualified Cryptol.Eval.Value as Cry

import Cryptol.Compiler.PP(pp)
import Cryptol.Compiler.Error
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Cry2IR.InstanceMap

-- | This is the implementation of the monad
type M =
  WithBase IO
    '[ ReaderT    CompilerContext
     , ExceptionT CompilerError
     , StateT     CompilerState
     ]

-- | Common compilation functionality.
newtype CryC a = CryC (M a)
  deriving (Functor,Applicative,Monad)
  via M

instance BaseM CryC CryC where
  inBase = id


-- | Context for compiler computations
newtype CompilerContext = CompilerContext
  { roLocalTypes    :: Map Cry.Name Cry.Schema
    -- ^ Cryptol types of local variables.
    -- We need this to compute the Cryptol types of things.
  }

-- | State of the compiler.
data CompilerState = CompilerState
  { rwModuleInput :: Cry.ModuleInput IO
  , rwWarnings    :: Cry.Logger

  , rwTypes       :: Maybe (Map Cry.Name Cry.Schema)
    -- ^ This caches the types of all top-level things we know about.
    -- It is computed the first time we try to access the types.
    -- It is cleared if new modules are loaded, so that it gets recomputed.

  , rwPrims       :: Maybe Prims
    -- ^ This caches the names of loaded primitives.
    -- These are computed the first time they are loaded and are
    -- cleared if new modules are loaded.
    -- The mapping is used to determined the Cryptol names assigned to
    -- various primitives.

  , rwNameGen     :: !Int
    -- ^ A seed for generating names.

  , rwCompiled :: Map Cry.Name (InstanceMap FunDecl)
  }

-- | Information about primitives
data Prims = Prims
  { primToName  :: Cry.PrimMap
  , nameToPrimV :: Map Cry.Name Cry.PrimIdent
  , nameToPrimT :: Map Cry.Name Cry.PrimIdent
  }

-- | Execute a computation. May throw `CompilerError` if things go wrong.
runCryC :: CryC a -> IO a
runCryC (CryC m) =
  Cry.withSolver (pure ()) tcSolverConfig \solver ->
  do env <- Cry.initialModuleEnv
     let initialContext =
           CompilerContext
             { roLocalTypes = Map.empty
             }
         initialState =
           CompilerState
              { rwWarnings = Cry.stderrLogger
              , rwTypes = Nothing
              , rwPrims = Nothing
              , rwNameGen = 0
              , rwCompiled = mempty
              , rwModuleInput =
                  Cry.ModuleInput
                     { Cry.minpCallStacks = False
                     , Cry.minpEvalOpts   = evalConfig
                     , Cry.minpByteReader = BS.readFile
                     , Cry.minpModuleEnv  = env
                     , Cry.minpTCSolver   = solver
                     }
              }
     (res,_) <- runM m initialContext initialState
     case res of
       Left err -> throwIO err
       Right a  -> pure a

  where
  tcSolverConfig  = Cry.defaultSolverConfig []
  evalConfig      = pure Cry.EvalOpts
                           { Cry.evalLogger  = Cry.quietLogger
                           , Cry.evalPPOpts  = Cry.defaultPPOpts
                           }


-- | Do some IO stuff.
doIO :: IO a ->  CryC a
doIO m = CryC (inBase m)


-- | Execute a Cryptol module system command
doModuleCmd :: Cry.ModuleCmd a -> CryC a
doModuleCmd cmd =
  do inp <- CryC (rwModuleInput <$> get)
     (result,warnings) <- doIO (cmd inp)
     case result of
       Left err -> throwError (LoadError err)
       Right (a,newEnv) ->
         do mapM_ (addWarning . LoadWarning) warnings
            CryC (sets_ \s -> s { rwTypes = Nothing
                                , rwPrims = Nothing
                                , rwCompiled = mempty
                                , rwModuleInput =
                                    (rwModuleInput s)
                                       { Cry.minpModuleEnv = newEnv }})
            pure a

-- | Load a module to the compiler's environment.
-- This will also load all of the module's dependencies.
loadModuleByPath :: FilePath -> CryC ()
loadModuleByPath path =
  do _ <- doModuleCmd (Cry.loadModuleByPath path)
     pure ()

-- | Report this warning
addWarning :: CompilerWarning -> CryC ()
addWarning w =
  do logger <- CryC (rwWarnings <$> get)
     doIO (Cry.logPrint logger (pp w))

-- | Abort execution with this error
throwError :: CompilerError -> CryC a
throwError e = CryC (raise e)

catchablePanic :: String -> [String] -> CryC a
catchablePanic m ms = throwError (CatchablePanic m ms)

-- | Run the computation, returning any errors tagged with `Left`.
catchError :: CryC a -> CryC (Either CompilerError a)
catchError (CryC m) = CryC ((Right <$> m) `MLib.handle` (pure . Left))

-- | Abort due to an unsupported feature.
unsupported :: Text -> CryC a
unsupported msg = throwError (Unsupported msg)

-- | Get all loaded modules.
-- These are in dependency oreder, where later modules only depend on
-- earlier ones.
getLoadedModules :: CryC [Cry.Module]
getLoadedModules =
  CryC (Cry.loadedNonParamModules . Cry.minpModuleEnv . rwModuleInput <$> get)

-- | Get the name of a built-in type constructor.
getPrimTypeName :: Cry.ModName -> Text -> CryC Cry.Name
getPrimTypeName mn t =
  do mp <- getPrims
     case Map.lookup (Cry.PrimIdent mn t) (Cry.primTypes (primToName mp)) of
       Just nm -> pure nm
       Nothing -> panic "getPrimTypeName"
                    [ "Unknown primitive"
                    , "Module: " ++ show (Cry.pp mn)
                    , "Primitive: " ++ show (Text.unpack t)
                    ]

-- | Get the name of a built-in function/value.
getPrimDeclName :: Cry.ModName -> Text -> CryC Cry.Name
getPrimDeclName mn t =
  do mp <- getPrims
     case Map.lookup (Cry.PrimIdent mn t) (Cry.primDecls (primToName mp)) of
       Just nm -> pure nm
       Nothing -> panic "getPrimDeclName"
                    [ "Unknown primitive"
                    , "Module: " ++ show (Cry.pp mn)
                    , "Primitive: " ++ show (Text.unpack t)
                    ]

-- | Check if the given name is a primitive value, and if so get the
-- primitive name.
isPrimDecl :: Cry.Name -> CryC (Maybe Cry.PrimIdent)
isPrimDecl nm = Map.lookup nm . nameToPrimV <$> getPrims

-- | Check if the given name is a primitive type, and if so get the
-- primitive name.
isPrimType :: Cry.Name -> CryC (Maybe Cry.PrimIdent)
isPrimType nm = Map.lookup nm . nameToPrimT <$> getPrims

-- | Get the map of loaded primitives.
getPrims :: CryC Prims
getPrims =
  do mb <- CryC (rwPrims <$> get)
     case mb of
       Just done -> pure done
       Nothing ->
         do mp <- doModuleCmd Cry.getPrimMap
            let ps = Prims { primToName  = mp
                           , nameToPrimV = inv (Cry.primDecls mp)
                           , nameToPrimT = inv (Cry.primTypes mp)
                           }
            CryC (sets_ \s -> s { rwPrims = Just ps })
            pure ps
  where
  inv mp = Map.fromList [ (x,y) | (y,x) <- Map.toList mp ]

-- | Get the types of all top-level loaded declarations.
getTopTypes :: CryC (Map Cry.Name Cry.Schema)
getTopTypes =
  do mb <- CryC (rwTypes <$> get)
     case mb of
       Just done -> pure done
       Nothing ->
         do ms <- getLoadedModules
            let mp = Map.fromList [ d | m <- ms, d <- declsTypesOf m ]
            CryC (sets_ \s -> s { rwTypes = Just mp })
            pure mp
  where
  declsTypesOf m =
    [ (Cry.dName d, Cry.dSignature d)
    | dg <- Cry.mDecls m
    , d  <- Cry.groupDecls dg
    ]


-- | Add some Crypotl locals for the duration of a compiler computation
withCryLocals :: [(Cry.Name, Cry.Type)] -> CryC a -> CryC a
withCryLocals locs (CryC m) = CryC (mapReader upd m)
  where
  locTs = Map.fromList [ (x, Cry.tMono t) | (x,t) <- locs ]
  upd ro = ro { roLocalTypes = Map.union locTs (roLocalTypes ro) }


-- | Get the types of everything in scope.
getTypes :: CryC (Map Cry.Name Cry.Schema)
getTypes =
  do locals  <- CryC (roLocalTypes <$> ask)
     globals <- getTopTypes
     -- XXX: maybe we should cache this?
     pure (Map.union locals globals)

-- | Get the type of an expression.
-- This will panic of the expression does not have a monomorphic type.
getTypeOf :: Cry.Expr -> CryC Cry.Type
getTypeOf expr =
  do env <- getTypes
     pure (Cry.fastTypeOf env expr)

-- | Get the schema of an expression.
getSchemaOf :: Cry.Expr -> CryC Cry.Schema
getSchemaOf expr =
  do env <- getTypes
     pure (Cry.fastSchemaOf env expr)

addCompiled :: Cry.Name -> InstanceMap FunDecl -> CryC ()
addCompiled x def =
  CryC $ sets_ \s -> s { rwCompiled = Map.insert x def (rwCompiled s) }

getCompiled :: CryC (Map Cry.Name (InstanceMap FunDecl))
getCompiled = CryC $ rwCompiled <$> get

getFun :: Cry.Name -> CryC (InstanceMap (FunName,FunType))
getFun x =
  do comp <- CryC (rwCompiled <$> get)
     let info d = (irfName d, irfType d)
     case Map.lookup x comp of
       Just fu -> pure (info <$> fu)
       Nothing -> catchablePanic "getFunType" [ "Missing function"
                                              , show (pp x)
                                              ]

newNameId :: CryC NameId
newNameId =
  CryC $ sets \rw -> let x = rwNameGen rw
                     in (AnonId x, rw { rwNameGen = x + 1 })


--------------------------------------------------------------------------------

getSolver :: CryC Cry.Solver
getSolver =
  do s <- CryC get
     pure (Cry.minpTCSolver (rwModuleInput s))
