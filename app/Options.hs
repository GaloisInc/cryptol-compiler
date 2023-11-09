module Options
  ( Options(..)
  , Command(..)
  , getOptions
  , showHelp
  ) where

import SimpleGetOpt

data Command =
    DefaultCommand
  | ListPrimitives
  | ShowHelp
    deriving Show

data Options = Options
  { optCommand    :: Command
  , optFiles      :: [FilePath]
  , optCrateName  :: String
  , optOutputPath :: FilePath
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optCommand  = DefaultCommand
  , optFiles    = []
  , optCrateName = "cryptol-gen"
  , optOutputPath = "cry-rust"
  }

options :: OptSpec Options
options = optSpec
  { progDescription = [ "A compiler for Cryptol" ]

  , progOptions =
      [ Option [] ["dbg-list-primitives"]
        "List declared primitves"
        $ NoArg \o -> Right o { optCommand = ListPrimitives }

      , Option [] ["help"]
        "Display this help"
        $ NoArg \o -> Right o { optCommand = ShowHelp }

      , Option ['o'] ["output"]
        ("Output directory for generated crate (default \"" ++ optCrateName defaultOptions ++ "\")")
        $ ReqArg "PATH" \s o -> Right o { optOutputPath = s }

      , Option [] ["crate"]
        ("Name of crate to be generated (default \"" ++ optCrateName defaultOptions ++ "\")")
        $ ReqArg "NAME" \s o -> Right o { optCrateName = s }
      ]

  , progParamDocs =
      [ ("FILES",   "The root Cryptol files to compile")
      ]

  , progParams = \p o -> Right o { optFiles = p : optFiles o }
  }


getOptions :: IO Options
getOptions = getOpts defaultOptions options

showHelp :: IO ()
showHelp = dumpUsage options
