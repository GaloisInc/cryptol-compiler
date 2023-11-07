module Golden where

import qualified System.Process as Process
import qualified System.IO.Temp as Temp

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden

import qualified Cryptol.Compiler.Rust.Compiler as Compiler
import Cryptol.Compiler.Monad (runCryC)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BSU8

tests :: IO Tasty.TestTree
tests =
  do  files <- findTestFiles
      let ts = runTest <$> files
      pure $ Tasty.testGroup "compiler golden tests" ts
  where
    expected cryPath = cryPath ++ ".expected"
    runTest :: FilePath -> Tasty.TestTree
    runTest cryPath =
          Golden.goldenVsString cryPath (expected cryPath) (runCompiledSimple cryPath)

compileAndRunRust :: [FilePath] -> FilePath -> IO String
compileAndRunRust cryFiles cratePath =
  do  runCryC $ Compiler.compileExample cryFiles "test" cratePath
      Process.readCreateProcess (cargoRun cratePath) ""
  where
    cargoRun cp =
      let p = Process.proc "cargo" ["run", "-q"]
      in p { Process.cwd = Just cp }

compileAndRunRust' :: [FilePath] -> IO String
compileAndRunRust' files =
  Temp.withSystemTempDirectory "tempcrate" (compileAndRunRust files)

runCompiledSimple ::  FilePath -> IO BS.ByteString
runCompiledSimple path =
  BSU8.fromString <$> compileAndRunRust' [path]

findTestFiles :: IO [FilePath]
findTestFiles = Golden.findByExtension [".cry"] "test/golden"
