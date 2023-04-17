module Main where

import Data.Text(Text)
import Control.Exception
import Control.Monad(forM_)
import Data.Map qualified as Map
import System.IO(hPrint,stderr)
import System.Exit

import Cryptol.Utils.Ident    qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.TCon qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
-- import Cryptol.Compiler.Simple
import Cryptol.Compiler.IR
import Cryptol.Compiler.Cry2IR.Specialize
import Cryptol.Compiler.Cry2IR.InstanceMap


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
          xs <- catchError (compilePrimDecl t)
          let ppOpt (inst, ft) = vcat [ pp inst, pp ft, "---" ]
          let doc = nest 2
                      case xs of
                        Right ok ->
                          let Right im = instanceMapFromList ok
                              nu :: Integer -> Either a (IRStreamSize Text)
                              nu = Right . IRSize . IRFixedSize

                              va :: Text -> IRStreamSize Text
                              va = IRSize . IRPolySize . (`IRSizeName` MemSize)

                              hard :: IRStreamSize Text
                              hard = IRSize (IRComputedSize Cry.TCSub [ va "X", va "Y" ])

                              args = [ Right IRInfSize, Right hard, Left TInteger ]
                              ans = lookupInstance args im
                          in pp im
                        Left err -> pp err
          doIO (print doc)

isPrel :: Cry.Name -> Bool
isPrel x = Cry.nameTopModule x `elem` [Cry.preludeName, Cry.floatName]

{-
doSimpleCompile :: CryC ()
doSimpleCompile =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do ds <- compileModule m
           forM_ ds \d ->
             doIO (print (pp d))
-}
