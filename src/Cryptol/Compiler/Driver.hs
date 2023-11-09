module Cryptol.Compiler.Driver where

import Data.Map qualified as Map
import Control.Monad(forM_)
import System.FilePath(addExtension,(</>))
import System.Directory(createDirectoryIfMissing)

import Language.Rust.Pretty qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry
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
     forM_ cryMods \m ->
        do decls <- cry2IR m
           ir2Rust (Cry.mName m) decls
     crate <- getCrateName
     dir   <- getOutputDir
     modInfo <- getRustInfo
     let mods = map Rust.extModuleName (Map.elems modInfo)
     doIO (Rust.mkCrate crate dir mods)

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
     doIO (putStrLn ("Compiling module: " ++ nm))
     doIO (putStrLn "Eta Expansion")
     tys <- getTypes
     m' <- doNameGen (\s -> etaModule tys s m)
     -- doIO (print (cryPP m'))
     doIO (putStrLn "Converting to IR")
     -- doIO (print (cryPP m'))
     Cry2IR.compileModule m'
     ds <- getCompiled
     clearCompiled
     pure (reverse ds)


-- | Compile some IR to Rust, saving things in the file system.
-- Modifies the state to track information about compiled modules.
ir2Rust :: Cry.ModName -> [FunDecl] -> CryC ()
ir2Rust m ds =
  do doIO (putStrLn "Generating Rust")
     extMods <- getRustInfo
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




