module Options
  ( Options(..)
  , getOptions
  ) where

import Control.Monad(when)
import System.Exit(exitSuccess,exitFailure)
import System.IO(stderr,hPutStrLn)

import SimpleGetOpt

data Options = Options
  { optShowHelp :: Bool
  , optFiles    :: [FilePath]
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optShowHelp = False
  , optFiles    = []
  }

options :: OptSpec Options
options = optSpec
  { progDescription = [ "A compiler for Cryptol" ]

  , progOptions =
      [ Option [] ["help"]
        "Display this help"
        $ NoArg \o -> Right o { optShowHelp = True }
      ]

  , progParamDocs =
      [ ("FILES",   "The root Cryptol files to compile")
      ]

  , progParams = \p o -> Right o { optFiles = p : optFiles o }
  }

getOptions :: IO Options
getOptions =
  do opts <- getOpts defaultOptions options

     when (optShowHelp opts)
       do dumpUsage options
          exitSuccess

     when (null (optFiles opts))
      do hPutStrLn stderr "No input files. See --help for command line options."
         exitFailure

     pure opts

