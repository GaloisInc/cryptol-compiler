-- | Utilities for generating crates and including the generated runtime libraries
module Cryptol.Compiler.Rust.Crate (mkCrate) where

import Data.FileEmbed qualified as Embed
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Monad(forM_,when)

import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Compiler.Rust.Names(cryptolCrateString)
import Cryptol.Compiler.Rust.ModuleTree

mapFst :: (a -> a) -> (a, b) -> (a, b)
mapFst f (a,b) = (f a, b)

mkCrateDir :: ByteString -> [(FilePath, ByteString)] -> [(FilePath, ByteString)]
mkCrateDir cargoToml srcDir = ("Cargo.toml", cargoToml):srcDir'
  where
    srcDir' = fmap (mapFst ("src" </>)) srcDir

rtsCrateDir :: [(FilePath, ByteString)]
rtsCrateDir =  mkCrateDir $(Embed.embedFile ("rts-rust" </> "Cargo.toml"))
                          $(Embed.embedDir ("rts-rust" </> "src") )

dwordCrateDir :: [(FilePath, ByteString)]
dwordCrateDir = mkCrateDir $(Embed.embedFile ("dword" </> "Cargo.toml"))
                           $(Embed.embedDir ("dword" </> "src") )

rtsPath :: FilePath
rtsPath = "deps" </> "rts-rust"

dwordPath :: FilePath
dwordPath = "deps" </> "dword"

crateToml :: String -> String
crateToml crateName =
  unlines
    [ "[package]"
    , "name    = " <> q crateName
    , "version = \"0.1.0\""
    , "edition = \"2021\""
    , "autobins = false"    -- Don't try to figure out which is a binary
    , "[lib]"
    , "path = \"src/lib.rs\""
    , "[dependencies]"
    , cryptolCrateString ++ " = { path = " <> q rtsPath <> "}"
    , "dword = { path = " <> q dwordPath <> "}"
    , "num = \"0.4.0\""
    ]
  where
    q s = "\"" <> s <> "\""

writeEmbedFile :: FilePath -> (FilePath, ByteString) -> IO ()
writeEmbedFile fp (fn, bytes) =
  do  createDirectoryIfMissing True (fp </> takeDirectory fn)
      BS.writeFile (fp </> fn) bytes

writeRtsTo :: FilePath -> IO ()
writeRtsTo fp = forM_ rtsCrateDir (writeEmbedFile fp)

writeDwordTo :: FilePath -> IO ()
writeDwordTo fp = forM_ dwordCrateDir (writeEmbedFile fp)


-- | Make a barebones crate with the given name and the given directory.
mkCrate ::
  Bool          {- ^ Do we want an example driver -} ->
  String        {- ^ Name of the crate -} ->
  FilePath      {- ^ Directory to store the crate in -} ->
  [[Rust.Ident]] ->
  IO ()
mkCrate withExe crateName target mods =
  do  createDirectoryIfMissing True target
      writeFile (target </> "Cargo.toml") toml
      writeRtsTo (target </> rtsPath)
      writeDwordTo (target </> dwordPath)

      let src = target </> "src"
      createDirectoryIfMissing True src

      addModuleDecls src mods
{-
      unless (null mods)
        do let lib = sourceFile Nothing [disableWarning "special_module_name"]
                                        (map pubMod mods)
           writeFile (src </> "lib.rs") (show (Rust.pretty' lib))
-}

      when withExe sampleDriver

  where
  baseToml = crateToml crateName
  toml
    | withExe = unlines [ baseToml
                        , "[[bin]]"
                        , "name = \"cry_test\""
                        , "path = \"exe/main.rs\""
                        ]
    | otherwise = baseToml


  sampleDriver =
    do let src = target </> "exe"
       createDirectoryIfMissing True src

       writeFile (src </> "main.rs") $
         unlines
           [ "use " ++ cryptolCrateString ++ "::trait_methods::*;"
           , "pub fn main() {"
           , "  println!(\"{:#x}\"," ++ crateName ++ "::main::main().display())"
           , "}"
           ]


{-
declareNestedModulesTree :: Set [Text] -> FilePath -> ModTree -> IO ()
declareNestedModulesTree genModules file outPath (Mod x cs) =
  let nested = [ c | Mod c _ <- cs ]
-}




