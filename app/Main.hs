module Main where

import Control.Exception
import Control.Monad(forM_)
import Data.Map qualified as Map
import System.IO(hPrint,stderr)
import System.Exit

import Cryptol.Utils.Ident    qualified as Cry
import Cryptol.TypeCheck.Solver.Numeric.Interval as Cry
import Cryptol.TypeCheck.PP qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.Simple
import Cryptol.Compiler.Interval


import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       do mapM_ loadModuleByPath (optFiles opts)
          doSimpleCompile
{-
          tys <- getTopTypes
          let isPrel x = Cry.nameTopModule x == Cry.preludeName
              nonPrel  = Map.filterWithKey (\k _ -> not (isPrel k)) tys
          doIO $ forM_ (Map.toList nonPrel) \(x,t) ->
            do let nm = Cry.addTNames (Cry.sVars t) Cry.emptyNameMap
               it <- schemaIntervals (2^(64::Int) - 1) t
               print (cryPP x $$ nest 2 (ppSizeMap nm it))
-}

  `catch` \e ->
    do hPrint stderr (pp (e :: CompilerError))
       exitFailure

doSimpleCompile =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do ds <- compileModule m
           forM_ ds \d ->
             doIO (print (pp d))
