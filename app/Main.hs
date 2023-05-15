module Main where

import Control.Exception
import Control.Monad(forM_)
import System.IO(hPrint,stderr)
import System.Exit
import Data.Map qualified as Map

import Language.Rust.Pretty qualified as Rust

import Cryptol.Utils.Ident    qualified as Cry
import Cryptol.TypeCheck.AST qualified  as Cry
import Cryptol.ModuleSystem.Name qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.Cry2IR.InstanceMap(instanceMapToList)
import Cryptol.Compiler.Cry2IR.Compile
import Cryptol.Compiler.Rust.CodeGen


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
        do doIO (putStrLn ("Compiling: " ++ show (cryPP (Cry.mName m))))
           compileModule m
           decls <- getCompiled
           let declList = concatMap instanceMapToList (Map.elems decls)
               gi = GenInfo { genCurModule = Cry.mName m
                            , genExternalModules = mempty
                            }
               srcFile  = genModule gi declList
           doIO (print (Rust.pretty' srcFile))

{-
           forM_ fs \(f,im) ->
             unless (isPrel f)
              do s <- getSchemaOf (Cry.EVar f)
                 doIO $ do putStrLn "---------"
                           print (cryPP f <+> ":" <+> cryPP s)
                           print (pp im)
                           putStrLn "---------"
-}




