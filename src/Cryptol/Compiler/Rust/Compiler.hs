module Cryptol.Compiler.Rust.Compiler where

import qualified Data.Map as Map
import Control.Monad (forM_)

import Cryptol.Compiler.Monad
   (CryC, getLoadedModules, doIO, getCompiled, clearCompiled, loadModuleByName,
    loadModuleByPath, floatName, preludeName, getTypes, doNameGen)
import Cryptol.Compiler.Rust.CodeGen (GenInfo(..), genModule)
import qualified Cryptol.TypeCheck.AST as Cry
import Cryptol.Compiler.PP (cryPP)
import Cryptol.Compiler.Cry2IR.Compile (compileModule)
import qualified Cryptol.Compiler.Rust.Crate as Crate
import Cryptol.IR.Eta(etaModule)

-- | Load Cryptol specs
loadInputs :: [FilePath] -> CryC ()
loadInputs files =
  case files of
    [] -> mapM_ loadModuleByName [ floatName, preludeName ]
    _  -> mapM_ loadModuleByPath files

-- | Compile a Cryptol module to IR.
-- The IR is stored in the monad.
doModule :: Cry.Module -> CryC ()
doModule m =
  do let nm = show (cryPP (Cry.mName m))
     doIO (putStrLn ("Compiling module: " ++ nm))
     doIO (putStrLn "Eta Expansion")
     tys <- getTypes
     m' <- doNameGen (\s -> etaModule tys s m)
     -- doIO (print (cryPP m'))
     doIO (putStrLn "Converting to IR")
     -- doIO (print (cryPP m'))
     compileModule m'
     ds <- getCompiled
     clearCompiled




compileLoadedModules :: CryC ()
compileLoadedModules = getLoadedModules >>= compileModules

compileModules :: [Cry.Module] -> CryC ()
compileModules ms =
   forM_ ms \m ->
      do doIO (putStrLn ("Converting to IR: " ++ show (cryPP (Cry.mName m))))
         compileModule m

doSimpleCompile :: String -> FilePath -> CryC ()
doSimpleCompile crateName outputPath =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do let nm = show (cryPP (Cry.mName m))
           doIO (putStrLn ("Processing module: " ++ nm))
           doIO (putStrLn "Eta Expansion")
           tys <- getTypes
           m' <- doNameGen (\s -> etaModule tys s m)
           -- doIO (print (cryPP m'))
           doIO (putStrLn "Converting to IR")
           -- doIO (print (cryPP m'))
           compileModule m'

     decls <- getCompiled
     let declList = concat (Map.elems decls)
         gi = GenInfo { genCurModule       = Cry.mName (last ms)
                      , genExternalModules = mempty
                      }

     (_,srcFile) <- doIO $ genModule gi declList

     doIO (putStrLn ("Saving output in " ++ show outputPath))
     doIO $ Crate.writeExampleCrate crateName outputPath srcFile



compileExample :: [FilePath] -> String -> FilePath -> CryC ()
compileExample cryFiles crateName outputPath =
   do mapM_ loadModuleByPath cryFiles
      doSimpleCompile crateName outputPath



