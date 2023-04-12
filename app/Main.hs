module Main where

import Control.Exception
import Control.Monad(forM_)
import Data.Map qualified as Map
import System.IO(hPrint,stderr)
import System.Exit

import Cryptol.Utils.Ident    qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.Simple
import Cryptol.Compiler.Cry2IR.Specialize
import Cryptol.Compiler.Cry2IR.InstanceMap
import Cryptol.Compiler.IR


import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       do mapM_ loadModuleByPath (optFiles opts)
          -- doSimpleCompile
          doTestSpec

  `catch` \e ->
    do hPrint stderr (pp (e :: CompilerError))
       exitFailure


doTestSpec :: CryC ()
doTestSpec =
  do tys <- getTopTypes
     let nonPrel  = Map.filterWithKey (\k _ -> not (isPrel k)) tys
     forM_ (Map.toList nonPrel) \(x,t) ->
       do doIO (print (cryPP x $$ nest 2 (cryPP t)))
          xs <- catchError (testSpec t)
          let ppOpt (inst, ft) = vcat [ pp inst, pp ft, "---" ]
          let doc = nest 2
                      case xs of
                        Right ok ->
                          let im = instanceMapFromList ok
                              nu :: Integer -> Either a StreamSize
                              nu = Right . IRSize . IRFixedSize
                              inf :: Either a StreamSize
                              inf = Right IRInfSize
                              args = [ inf, nu 16, Left TInteger ]
                              ans = lookupInstance args im
                          in pp ans
                        Left err -> pp err
          doIO (print doc)

isPrel :: Cry.Name -> Bool
isPrel x = Cry.nameTopModule x `elem` [Cry.preludeName, Cry.floatName]

doSimpleCompile :: CryC ()
doSimpleCompile =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do ds <- compileModule m
           forM_ ds \d ->
             doIO (print (pp d))
