module Main where

import Options

main :: IO ()
main =
  do opts <- getOptions
     print opts

