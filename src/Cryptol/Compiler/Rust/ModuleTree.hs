module Cryptol.Compiler.Rust.ModuleTree (addModuleDecls) where

import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map
import System.FilePath(joinPath, (<.>), (</>))

import Language.Rust.Pretty qualified as Rust
import Language.Rust.Data.Ident qualified as Rust
import Cryptol.Compiler.Rust.Utils

type ModForest = [ModTree]
data ModTree   = Mod Rust.Ident ModForest

-- | For each module, what modules are immediately contained in it.
newtype ModMap = ModMap (Map Rust.Ident ModMap)


instance Semigroup ModMap where
  ModMap xs <> ModMap ys = ModMap (Map.unionWith (<>) xs ys)

instance Monoid ModMap where
  mempty = ModMap mempty

fromPath :: [Rust.Ident] -> ModMap
fromPath xs =
  case xs of
    y : ys -> ModMap (Map.singleton y (fromPath ys))
    []     -> mempty

makeModMap :: [[Rust.Ident]] -> ModMap
makeModMap = mconcat . map fromPath

modMapToForest :: ModMap -> ModForest
modMapToForest (ModMap mp) =
  [ Mod x (modMapToForest xs) | (x,xs) <- Map.toList mp ]

addModuleDecls :: FilePath -> [[Rust.Ident]] -> IO ()
addModuleDecls dir genModules = declareNested dir set [] forest
  where
  forest = modMapToForest (makeModMap genModules)
  set    = Set.fromList genModules

declareNested ::
  FilePath -> Set [Rust.Ident] -> [Rust.Ident] -> ModForest -> IO ()
declareNested dir genModules outPath fs =
  do modifyFile outFile decls
     mapM_ doNext fs
  where
  doNext (Mod i more) = declareNested dir genModules (i : outPath) more

  rev :: [Rust.Ident]
  rev = reverse outPath

  outFile :: FilePath
  outFile = dir </>
            case map Rust.name rev of
              []   -> "lib.rs"
              path -> joinPath path <.> "rs"

  decls = show $ Rust.pretty'
        $ sourceFile Nothing attrs [ pubMod i | Mod i _ <- fs ]

  attrs = case rev of
            [] -> [disableWarning "special_module_name"]
            _  -> []

  modifyFile
    | rev `Set.member` genModules  = appendFile
    | otherwise                    = writeFile




