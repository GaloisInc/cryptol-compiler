module Golden where

import qualified System.Process as Process
import qualified System.IO.Temp as Temp

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.ExpectedFailure as Tasty
import qualified Test.Tasty.Golden as Golden

import qualified Cryptol.Compiler.Driver as Compiler
import Cryptol.Compiler.Monad (runCryC)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BSU8
import qualified System.FilePath as FilePath
import qualified Data.List as List

tests :: IO Tasty.TestTree
tests =
  do  files <- findTestFiles
      let ts = runTest <$> files
      pure $ Tasty.testGroup "compiler golden tests" ts
  where
    expected cryPath = cryPath ++ ".expected"
    runTest :: FilePath -> Tasty.TestTree
    runTest cryPath =
      expectFailOrSucceed cryPath $
      Golden.goldenVsString cryPath (expected cryPath)
                                    (runCompiledSimple cryPath)

    expectFailOrSucceed :: FilePath -> Tasty.TestTree -> Tasty.TestTree
    expectFailOrSucceed cryPath
      | cryPath `List.elem` zTypeTests
      = Tasty.expectFail
      | otherwise
      = id

    -- The following test cases rely on Z types, which aren't currently
    -- supported (#34).
    zTypeTests :: [FilePath]
    zTypeTests =
      [ "test/golden/core/test_ring_add_z7.cry"
      , "test/golden/core/test_ring_mul_z7.cry"
      , "test/golden/core/test_ring_negate_z7.cry"
      , "test/golden/core/test_ring_pow_z7.cry"
      , "test/golden/core/test_ring_sub_z7.cry"
      ]

compileAndRunRust :: [FilePath] -> FilePath -> IO String
compileAndRunRust cryFiles cratePath =
  do  runCryC cratePath "test" ["Main"] (Compiler.cry2rust cryFiles)
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
findTestFiles =
  do  files <- Golden.findByExtension [".cry"] "test/golden"
      pure $ filter (\p -> "test_" `List.isPrefixOf` FilePath.takeBaseName p) files

