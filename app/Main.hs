module Main where

import Control.Exception
import Control.Monad(forM_,unless)
import Data.Map qualified as Map
import System.IO(hPrint,stderr)
import System.Exit

import Cryptol.Utils.Ident    qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.Cry2IR.Compile


import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       do mapM_ loadModuleByPath (optFiles opts)
          doSimpleCompile

  `catch` \e ->
    do hPrint stderr (pp (e :: CompilerError))
       exitFailure


isPrel :: Cry.Name -> Bool
isPrel x = Cry.nameTopModule x `elem` [Cry.preludeName, Cry.floatName]

doSimpleCompile :: CryC ()
doSimpleCompile =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do compileModule m
           fs <- Map.toList <$> getCompiled
           forM_ fs \(f,im) ->
             unless (isPrel f)
              do s <- getSchemaOf (Cry.EVar f)
                 doIO $ do putStrLn "---------"
                           print (cryPP f <+> ":" <+> cryPP s)
                           print (pp im)
                           putStrLn "---------"




