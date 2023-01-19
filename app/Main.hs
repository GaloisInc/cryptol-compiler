module Main where

import Control.Exception
import qualified Data.Map as Map
import System.IO(hPrint,stderr)
import System.Exit

import qualified Cryptol.Utils.Ident as Cry
import qualified Cryptol.Utils.PP as Cry
import qualified Cryptol.TypeCheck.AST as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.Simple


import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       do mapM_ loadModuleByPath (optFiles opts)
          ms <- getLoadedModules
          doIO (mapM_ (print . Cry.pp . Cry.mName) ms)
  `catch` \e ->
    do hPrint stderr (pp (e :: CompilerError))
       exitFailure

