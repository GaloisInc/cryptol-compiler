module Cryptol.Compiler.Rust.Compiler where
import Cryptol.Compiler.Monad (CryC, getLoadedModules, doIO, getCompiled, loadModuleByName, loadModuleByPath, floatName, preludeName)
import Cryptol.Compiler.Rust.CodeGen (GenInfo(..), genModule)
import Control.Monad (forM_)
import qualified Cryptol.TypeCheck.AST as Cry
import Cryptol.Compiler.PP (cryPP)
import Cryptol.Compiler.Cry2IR.Compile (compileModule)
import Cryptol.Compiler.Cry2IR.InstanceMap (instanceMapToList)
import qualified Data.Map as Map
import System.FilePath ((</>))
import qualified Cryptol.Compiler.Rust.Crate as Crate

loadInputs :: [FilePath] -> CryC ()
loadInputs files =
  case files of
    [] -> mapM_ loadModuleByName [ floatName, preludeName ]
    _  -> mapM_ loadModuleByPath files

compileLoadedModules :: CryC ()
compileLoadedModules = getLoadedModules >>= compileModules

compileModules :: [Cry.Module] -> CryC ()
compileModules ms =
   forM_ ms \m ->
      do doIO (putStrLn ("Converting to IR: " ++ show (cryPP (Cry.mName m))))
         compileModule True m

doSimpleCompile :: String -> FilePath -> CryC FilePath
doSimpleCompile crateName outputPathBase =
  do ms <- getLoadedModules
     compileModules ms
     decls <- getCompiled
     let declList = concatMap instanceMapToList (Map.elems decls)
         gi = GenInfo { genCurModule       = Cry.mName (last ms)
                      , genExternalModules = mempty
                      }

     srcFile <- doIO $ genModule gi declList

     let outputPath = outputPathBase </> crateName
     doIO (Crate.writeExampleCrate crateName srcFile outputPath)
     pure outputPath
   where
      isPreludeModule m = Cry.mName m `elem` [preludeName, floatName]
      loadPrelude ms = undefined

compileExample :: [FilePath] -> String -> FilePath -> CryC FilePath
compileExample cryFiles crateName outputPath =
   do mapM_ loadModuleByPath cryFiles
      doSimpleCompile crateName outputPath
