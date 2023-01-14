module Main where

import qualified Data.Map as Map

import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP

import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       do mapM_ loadModuleByPath (optFiles opts)
          x   <- getPrimDeclName preludeName "number"
          tys <- getTypes
          case Map.lookup x tys of
            Just ty -> doIO (print (cryPP ty))
            Nothing -> panic "Missing type" []

