module Cryptol.Compiler.Driver where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Maybe(mapMaybe)
import Control.Monad(forM_)
import System.FilePath(addExtension,(</>))
import System.Directory(createDirectoryIfMissing)

import Language.Rust.Pretty qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.ModuleSystem.Exports qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.IR.FreeVars qualified as Cry
import Cryptol.IR.Eta(etaModule)

import Cryptol.Compiler.PP
import Cryptol.Compiler.Monad
import Cryptol.Compiler.IR.Cryptol

import Cryptol.Compiler.Cry2IR.Compile qualified as Cry2IR
import Cryptol.Compiler.Rust.CodeGen qualified as Rust
import Cryptol.Compiler.Rust.Crate qualified as Rust


-- | Load some Cryptol specs and generate Rust for them.
cry2rust :: [FilePath] -> CryC ()
cry2rust files =
  do loadInputs files
     cryMods <- getLoadedModules
     entries <- Set.fromList . map Text.pack <$> getEntryModules
     forM_ (cryFilterDecls entries cryMods) \m ->
       do decls <- cry2IR m
          ir2Rust (Cry.mName m) decls

     crate <- getCrateName
     dir   <- getOutputDir
     modInfo <- getRustInfo
     let mods = map Rust.extModuleName (Map.elems modInfo)
     doIO (Rust.mkCrate True crate dir mods)

-- | Get public roots for this module, if any.
cryModRoots :: Set Text -> Cry.Module -> [Cry.Name]
cryModRoots entMods m
  | Cry.modNameToText (Cry.mName m) `Set.member` entMods =
    let ex  = Cry.exported Cry.NSValue (Cry.mExports m)
        ds  = concatMap Cry.groupDecls (Cry.mDecls m)
    in [ x | d <- ds, let x = Cry.dName d, x `Set.member` ex ]
  | otherwise = []

-- | Only keep declarations relevant for the given entry modules.
cryFilterDecls :: Set Text -> [Cry.Module] -> [Cry.Module]
cryFilterDecls entMods ms = map filterMod ms
  where
  roots    = concatMap (cryModRoots entMods) ms
  rootDeps = go Set.empty roots

  go !used todo =
    case todo of
      [] -> used
      x : xs
        | x `Set.member` used -> go used xs
        | otherwise ->
          case Map.lookup x allDeps of
            Just ds -> go (Set.insert x used) (ds ++ xs)
            Nothing -> panic "cryFilterDeps" ["Missing dependencies"]

  allDeps = Map.unions (map mDeps ms)
  mDeps m = Map.fromList (concatMap dgDeps (Cry.mDecls m))
  dgDeps dg =
    case dg of
      Cry.NonRecursive d -> [dDeps d]
      Cry.Recursive ds -> map dDeps ds
  dDeps d = (Cry.dName d, Set.toList (Cry.valDeps (Cry.freeVars d)))

  keep d = Cry.dName d `Set.member` rootDeps
  filterMod m = m { Cry.mDecls = mapMaybe filterDG (Cry.mDecls m) }
  filterDG dg =
    case dg of
      Cry.Recursive ds ->
        case filter keep ds of
          [] -> Nothing
          ds' -> Just (Cry.Recursive ds')
      Cry.NonRecursive d
        | keep d -> Just (Cry.NonRecursive d)
        | otherwise -> Nothing


-- | Load Cryptol specs
loadInputs :: [FilePath] -> CryC ()
loadInputs files =
  case files of
    [] -> mapM_ loadModuleByName [ floatName, preludeName ]
    _  -> mapM_ loadModuleByPath files

-- | Compile a Cryptol module to IR.
-- The state of the monad is updated to keep track of the instances we
-- generated.
cry2IR :: Cry.Module -> CryC [FunDecl]
cry2IR m =
  do let nm = show (cryPP (Cry.mName m))
     tys <- getTypes
     m' <- doNameGen (\s -> etaModule tys s m)
     -- doIO (print (cryPP m'))
     -- doIO (print (cryPP m'))
     Cry2IR.compileModule m'
     ds <- getCompiled
     clearCompiled
     pure (reverse ds)


-- | Compile some IR to Rust, saving things in the file system.
-- Modifies the state to track information about compiled modules.
ir2Rust :: Cry.ModName -> [FunDecl] -> CryC ()
ir2Rust m ds =
  do extMods <- getRustInfo
     let gi =
          Rust.GenInfo
            { Rust.genCurModule       = m
            , Rust.genExternalModules = extMods
            }
     (extM,rustFile) <- doIO (Rust.genModule gi ds)
     addRustInfo m extM
     dir <- getOutputDir
     let rootName = show (Rust.pretty' (Rust.extModuleName  extM))
         srcDir   = dir </> "src"
         filePath = srcDir </> addExtension rootName "rs"
     doIO
       do createDirectoryIfMissing True srcDir
          writeFile filePath (show (Rust.pretty' rustFile))




