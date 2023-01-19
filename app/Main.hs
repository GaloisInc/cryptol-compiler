module Main where

import qualified Data.Map as Map

import qualified Cryptol.TypeCheck.AST as Cry

import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP

import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       do mapM_ loadModuleByPath (optFiles opts)
          x   <- getPrimDeclName preludeName "number"
          ty  <- getSchemaOf (Cry.EVar x)
          doIO (print (cryPP ty))

