-- | The compiler monad, used to keep track of state, configuration, errors,
-- etc.
module Cryptol.Compiler.Monad
  (
    -- * Compiler Monad
    CryC, runCryC

    -- * Loaded Modules
  , loadModuleByPath
  , loadModuleByName
  , getLoadedModules

    -- * Names of Primitives
  , getPrimTypeName
  , getPrimDeclName
  , isPrimDecl
  , isPrimType
  , listPrims
  , Cry.preludeName
  , Cry.floatName

    -- * Errors and Warnings
  , enterLoc
  , addWarning
  , throwError
  , catchError
  , unsupported
  , panic
  , catchablePanic
  , enableWarningOutput

    -- * IO
  , doIO

    -- * Locals
  , withCryLocals

    -- * Types
  , getTypes
  , getTypeOf
  , getSchemaOf
  , getTopTypes
  , getFun
  , FunIface(..), AssumeSmall(..), noAssumeSmall

    -- * Name generation
  , newNameId
  , doNameGen

    -- * IR generation
  , addCompiled
  , getCompiled
  , clearCompiled

    -- * Rust generation
  , addRustInfo
  , getRustInfo

    -- * SMT solver
  , getSolver

    -- * Configuratoin
  , getOutputDir
  , getCrateName
  , getEntryModules
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Control.Exception
import MonadLib
import MonadLib qualified as MLib
import qualified Data.ByteString as BS
import Data.List(foldl')
import Data.Map(Map)
import qualified Data.Map as Map

import qualified Cryptol.ModuleSystem as Cry
import qualified Cryptol.ModuleSystem.Name as Cry
import qualified Cryptol.ModuleSystem.Env as Cry
import qualified Cryptol.TypeCheck.InferTypes as Cry
import qualified Cryptol.TypeCheck.Solver.SMT as Cry
import qualified Cryptol.TypeCheck.AST as Cry
import qualified Cryptol.TypeCheck.TypeOf as Cry
import qualified Cryptol.Utils.Ident as Cry
import qualified Cryptol.Utils.PP as Cry
import qualified Cryptol.Utils.Logger as Cry
import qualified Cryptol.Eval.Value as Cry

import Cryptol.Compiler.PP(pp,Doc)
import Cryptol.Compiler.Error hiding (unsupported)
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Cry2IR.InstanceMap
import Cryptol.Compiler.Rust.Monad qualified as Rust

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
data CompilerContext = CompilerContext
  { roLocalTypes    :: Map Cry.Name Cry.Schema
    -- ^ Cryptol types of local variables.
    -- We need this to compute the Cryptol types of things.

  , roLoc           :: !Loc
    -- ^ Location for error reporting

  , roOutputDir     :: !FilePath
    -- ^ Write output in here

  , roCrateName     :: !String
    -- ^ Name of the crete we wnat to generate

  , roEntryModules  :: [String]
    -- ^ Entry points for the code.
    -- We only generate code that is needed to support these.

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

  , rwFuns        :: !(Map Cry.Name (InstanceMap FunIface))
    -- ^ Known instance for each Cryptol function

  , rwCompiled    :: ![FunDecl]
    -- ^ Declarations in IR form (output of Cry2IR is collected here)

  , rwRustModInfo :: !(Map Cry.ModName Rust.ExtModule)
    -- ^ Information about modules already compiled to Rust (IR2Rust)

  }

data FunIface = FunIface {
  fiName  :: FunName,
  fiType  :: FunType,
  fiSmall :: AssumeSmall
}

data AssumeSmall = AssumeSmall
  { asForall :: [Cry.TParam]
  , asSizes  :: [Cry.Type]
    -- ^ Cryptol types of kind #.  It is only safe to call this functino,
    -- if the given numeric types are small enough to fit in MemSize
  }

noAssumeSmall :: AssumeSmall
noAssumeSmall = AssumeSmall
  { asForall = []
  , asSizes = []
  }



-- | Information about primitives
data Prims = Prims
  { primToName  :: Cry.PrimMap
  , nameToPrimV :: Map Cry.Name Cry.PrimIdent
  , nameToPrimT :: Map Cry.Name Cry.PrimIdent
  }

-- | Execute a computation. May throw `CompilerError` if things go wrong.
runCryC :: FilePath -> String -> [String] -> CryC a -> IO a
runCryC outDir crateName ents (CryC m) =
  Cry.withSolver (pure ()) tcSolverConfig \solver ->
  do env <- Cry.initialModuleEnv
     let initialContext =
           CompilerContext
             { roLocalTypes = Map.empty
             , roLoc        = []
             , roOutputDir  = outDir
             , roCrateName  = crateName
             , roEntryModules = ents
             }
         initialState =
           CompilerState
              { rwWarnings = Cry.quietLogger
              , rwTypes = Nothing
              , rwPrims = Nothing
              , rwNameGen = 0
              , rwFuns = mempty
              , rwCompiled = mempty
              , rwRustModInfo = mempty
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
  tcSolverConfig  = (Cry.defaultSolverConfig []) { Cry.solverVerbose = 0 }
  evalConfig      = pure Cry.EvalOpts
                           { Cry.evalLogger  = Cry.quietLogger
                           , Cry.evalPPOpts  = Cry.defaultPPOpts
                           }

-- | Enable writing compilation warnings to stderr
enableWarningOutput :: CryC ()
enableWarningOutput =
  CryC $ sets_ \s -> s { rwWarnings = Cry.stderrLogger }

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

doNameGen :: (Cry.Supply -> (a,Cry.Supply)) -> CryC a
doNameGen f = CryC $ sets \s ->
  let inp = rwModuleInput s
      (a,newEnv) = doIt inp
  in (a, s { rwModuleInput = inp { Cry.minpModuleEnv = newEnv }})
  where
  doIt inp =
    let env  = Cry.minpModuleEnv inp
        seed = Cry.meSupply env
        (a,newSeed) = f seed
    in (a, env { Cry.meSupply = newSeed })

-- | Load a module to the compiler's environment.
-- This will also load all of the module's dependencies.
loadModuleByPath :: FilePath -> CryC ()
loadModuleByPath path =
  do _ <- doModuleCmd (Cry.loadModuleByPath path)
     pure ()

-- | Load a module to the compiler's environment.
-- This will also load all of the module's dependencies.
loadModuleByName :: Cry.ModName -> CryC ()
loadModuleByName name =
  do _ <- doModuleCmd (Cry.loadModuleByName name)
     pure ()



-- | Report this warning
addWarning :: CompilerWarning -> CryC ()
addWarning w =
  do loc <- roLoc <$> CryC ask
     logger <- CryC (rwWarnings <$> get)
     doIO (Cry.logPrint logger (ppWithLoc loc (pp w)))

-- | Abort execution with this error
throwError :: CompilerError -> CryC a
throwError e = CryC (raise e)

catchablePanic :: String -> [String] -> CryC a
catchablePanic m ms = throwError (CatchablePanic m ms)

-- | Run the computation, returning any errors tagged with `Left`.
catchError :: CryC a -> CryC (Either CompilerError a)
catchError (CryC m) = CryC ((Right <$> m) `MLib.handle` (pure . Left))

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

-- | The names of all loaded primitive values.
listPrims :: CryC [Cry.Name]
listPrims =
  do ps <- getPrims
     pure (Map.keys (nameToPrimV ps))

-- | Get the map of loaded primitives.
getPrims :: CryC Prims
getPrims =
  do mb <- CryC (rwPrims <$> get)
     case mb of
       Just done -> pure done
       Nothing ->
         do ms <- getLoadedModules
            let (nameTs,nameVs) = foldl' primsFromMod (mempty,mempty) ms
                ps = Prims
                        { primToName =
                             Cry.PrimMap
                               { Cry.primDecls = inv nameVs
                               , Cry.primTypes = inv nameTs
                               }
                        , nameToPrimV = nameVs
                        , nameToPrimT = nameTs
                        }
            CryC (sets_ \s -> s { rwPrims = Just ps })
            pure ps
  where
  inv mp = Map.fromList [ (y,x) | (x,y) <- Map.toList mp ]

  primsFromMod ps m =
    let vs = foldl' addDeclVs ps (Cry.mDecls m)
        primTs = [ x | (x,def) <- Map.toList (Cry.mNominalTypes m)
                     , Cry.Abstract <- [Cry.ntDef def]
                 ]
    in foldl' addNameT vs primTs

  addDeclVs ps d =
    case d of
      Cry.Recursive ds -> foldl' addNameV ps (map Cry.dName ds)
      Cry.NonRecursive de -> addNameV ps (Cry.dName de)

  addNameV (ts,vs) x =
    case addName vs x of
      Just vs1 -> (ts,vs1)
      Nothing  -> (ts,vs)

  addNameT (ts,vs) x =
    case addName ts x of
      Just ts1 -> (ts1,vs)
      Nothing  -> (ts,vs)

  addName m x =
    do pm <- Cry.asPrim x
       pure (Map.insert x pm m)


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

addCompiled :: Cry.Name -> InstanceMap (FunDecl, AssumeSmall) -> CryC ()
addCompiled x def =
  CryC $ sets_ \s -> s { rwCompiled = newDecls ++ rwCompiled s
                       , rwFuns     = Map.insert x newKnown (rwFuns s)
                       }
  where
  info (d,small)   = FunIface { fiName = irfName d,
                                fiType = irfType d,
                                fiSmall = small
                               }
  newKnown = info <$> def
  newDecls = map fst (instanceMapToList def)

getCompiled :: CryC [FunDecl]
getCompiled = CryC $ rwCompiled <$> get

clearCompiled :: CryC ()
clearCompiled = CryC $ sets_ \rw -> rw { rwCompiled = mempty }

getFun :: Cry.Name -> CryC (InstanceMap FunIface)
getFun x =
  do comp <- CryC (rwFuns <$> get)
     case Map.lookup x comp of
       Just fu -> pure fu
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


--------------------------------------------------------------------------------

-- | Change the error location for the duration of the given computation.
enterLoc :: Loc -> CryC a -> CryC a
enterLoc loc (CryC m) =
  CryC (mapReader (\ro -> ro { roLoc = loc ++ roLoc ro }) m)

-- | Abort: we found something that's unsupported.
unsupported :: Doc -> CryC a
unsupported x =
  do loc <- roLoc <$> CryC ask
     throwError (Unsupported (reverse loc) x)


--------------------------------------------------------------------------------

addRustInfo :: Cry.ModName -> Rust.ExtModule -> CryC ()
addRustInfo m i =
  CryC (sets_ \rw -> rw { rwRustModInfo = Map.insert m i (rwRustModInfo rw) })

getRustInfo :: CryC (Map Cry.ModName Rust.ExtModule)
getRustInfo = CryC (rwRustModInfo <$> get)


--------------------------------------------------------------------------------

getOutputDir :: CryC FilePath
getOutputDir = CryC (roOutputDir <$> ask)

getCrateName :: CryC String
getCrateName = CryC (roCrateName <$> ask)

getEntryModules :: CryC [String]
getEntryModules = CryC (roEntryModules <$> ask)




