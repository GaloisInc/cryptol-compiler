-- | The compiler monad, used to keep track of state, configuration, errors,
-- etc.
module Cryptol.Compiler.Monad
  ( CryC
  , runCryC
  , loadModuleByPath
  , addWarning
  , throwError
  , doIO
  ) where

import Control.Exception
import MonadLib
import qualified Data.ByteString as BS

import qualified Cryptol.ModuleSystem as Cry
import qualified Cryptol.ModuleSystem.Env as Cry
import qualified Cryptol.TypeCheck.InferTypes as Cry
import qualified Cryptol.TypeCheck.Solver.SMT as Cry
import qualified Cryptol.TypeCheck.AST as Cry
import qualified Cryptol.Utils.PP as Cry
import qualified Cryptol.Utils.Logger as Cry
import qualified Cryptol.Eval.Value as Cry

import Cryptol.Compiler.PP(pp)
import Cryptol.Compiler.Error

-- | This is the implementation of the monad
type M = ExceptionT CompilerError (StateT CompilerState IO)

-- | Common compilation functionality.
newtype CryC a = CryC (M a)
  deriving (Functor,Applicative,Monad)
  via M

-- | State of the compiler.
data CompilerState = CompilerState
  { rwModuleInput :: Cry.ModuleInput IO
  , rwWarnings    :: Cry.Logger
  }

-- | Execute a computation. May throw `CompilerError` if things go wrong.
runCryC :: CryC a -> IO a
runCryC (CryC m) =
  Cry.withSolver (pure ()) tcSolverConfig \solver ->
  do env <- Cry.initialModuleEnv
     let initialState =
           CompilerState
              { rwWarnings = Cry.stderrLogger
              , rwModuleInput =
                  Cry.ModuleInput
                     { Cry.minpCallStacks = False
                     , Cry.minpEvalOpts   = evalConfig
                     , Cry.minpByteReader = BS.readFile
                     , Cry.minpModuleEnv  = env
                     , Cry.minpTCSolver   = solver
                     }
              }
     (res,_) <- runStateT initialState (runExceptionT m)
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
            CryC (sets_ \s -> s { rwModuleInput =
                                    (rwModuleInput s)
                                       { Cry.minpModuleEnv = newEnv }})
            pure a

-- | Load a module to the compiler's environment.
-- This will also load all of the module's dependencies.
loadModuleByPath :: FilePath -> CryC (Cry.ModulePath, Cry.TCTopEntity)
loadModuleByPath path =
  doModuleCmd (Cry.loadModuleByPath path)

-- | Report this warning
addWarning :: CompilerWarning -> CryC ()
addWarning w =
  do logger <- CryC (rwWarnings <$> get)
     doIO (Cry.logPrint logger (pp w))

-- | Abort execution with this error
throwError :: CompilerError -> CryC a
throwError e = CryC (raise e)


