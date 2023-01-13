-- | The compiler's errors and warnings.
module Cryptol.Compiler.Error where

import Control.Exception

import qualified Cryptol.ModuleSystem as Cry
import qualified Cryptol.Utils.PP as Cry

import Cryptol.Compiler.PP


-- | Compiler errors.
data CompilerError =
  LoadError Cry.ModuleError     -- ^ Error loading a Cryptol module
  deriving Show

-- | Compiler warnings.
data CompilerWarning =
  LoadWarning Cry.ModuleWarning   -- ^ Warnings when loading Cryptol modules

instance Exception CompilerError

instance PP CompilerError where
  pp err =
    case err of
      LoadError e -> pp (Cry.pp e)

instance PP CompilerWarning where
  pp warn =
    case warn of
      LoadWarning w -> pp (Cry.pp w)
