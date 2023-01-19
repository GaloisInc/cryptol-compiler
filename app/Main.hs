module Main where

import qualified Data.Map as Map

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
          mapM_ compileModule ms

