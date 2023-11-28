-- | The compiler's errors and warnings.
module Cryptol.Compiler.Error
  ( module Cryptol.Compiler.Error
  , panic
  ) where

import Data.Text qualified as Text
import Data.List(intersperse)
import Control.Exception

import qualified Cryptol.ModuleSystem as Cry
import qualified Cryptol.Utils.PP as Cry
import Cryptol.Utils.Panic (panic)

import Cryptol.Compiler.PP


-- | Compiler errors.
data CompilerError =
    LoadError Cry.ModuleError     -- ^ Error loading a Cryptol module
  | Unsupported Loc Doc           -- ^ Something we do not support
  | CatchablePanic String [String]
  deriving Show

-- | A way to identify a location in the program.
type Loc = [Doc]

-- | Compiler warnings.
data CompilerWarning =
    LoadWarning Cry.ModuleWarning   -- ^ Warnings when loading Cryptol modules
  | WarnError CompilerError

instance Exception CompilerError

instance PP CompilerError where
  pp err =
    case err of
      LoadError e -> pp (Cry.pp e)
      Unsupported loc feature ->
        case loc of
          [] -> msg
          _  -> hang locDoc 2 msg
        where
        locDoc = hsep (intersperse "/" loc)
        msg = "Unsupported feature:" $$ nest 2 feature

      CatchablePanic m ms ->
         ("PANIC:" <+> pp (Text.pack m)) $$
            nest 2 (vcat (map (pp . Text.pack) ms))


instance PP CompilerWarning where
  pp warn =
    case warn of
      LoadWarning w -> pp (Cry.pp w)
      WarnError e -> "[WARNING] Ignoring error:" $$ nest 2 (pp e)

unsupported :: [Doc] -> Doc -> a
unsupported loc what = throw (Unsupported loc what)

unknownLoc :: Loc
unknownLoc = []

