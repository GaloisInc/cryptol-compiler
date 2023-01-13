module Main where

import Cryptol.Compiler.Monad

import Options


main :: IO ()
main =
  do opts <- getOptions
     mods <- runCryC (mapM loadModuleByPath (optFiles opts))
     mapM_ print mods

