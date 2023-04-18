-- | The compiler's errors and warnings.
module Cryptol.Compiler.Error
  ( module Cryptol.Compiler.Error
  , panic
  ) where

import Data.Text(Text)
import Data.Text qualified as Text
import Control.Exception

import qualified Cryptol.ModuleSystem as Cry
import qualified Cryptol.Utils.PP as Cry
import Cryptol.Utils.Panic (panic)

import Cryptol.Compiler.PP


-- | Compiler errors.
data CompilerError =
    LoadError Cry.ModuleError     -- ^ Error loading a Cryptol module
  | Unsupported Text              -- ^ Something we do not support
  | CatchablePanic String [String]
  deriving Show

-- | Compiler warnings.
data CompilerWarning =
  LoadWarning Cry.ModuleWarning   -- ^ Warnings when loading Cryptol modules

instance Exception CompilerError

instance PP CompilerError where
  pp err =
    case err of
      LoadError e -> pp (Cry.pp e)
      Unsupported feature -> "Unsupported feature:" $$ nest 2 (pp feature)
      CatchablePanic m ms ->
         ("PANIC:" <+> pp (Text.pack m)) $$
            nest 2 (vcat (map (pp . Text.pack) ms))


instance PP CompilerWarning where
  pp warn =
    case warn of
      LoadWarning w -> pp (Cry.pp w)
