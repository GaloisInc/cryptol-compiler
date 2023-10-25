
-- | Utilities for generating crates and including the generated runtime libraries
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Cryptol.Compiler.Rust.Crate(
  writeExampleCrate
) where

import Data.FileEmbed qualified as Embed
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Monad(forM_)
import Language.Rust.Pretty qualified as RustPP

import Language.Rust.Syntax qualified as Rust

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
    , "[dependencies]"
    , "cryptol = { path = " <> q rtsPath <> "}"
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

-- | `writeExampleCrate crateName sourceFile targetDir`  writes a single rust
--   source file to a new crate named `crateName` locaated at `targetDir` and adds a
--   `fn main` to invoke the `cry_main` function and print the result.
--
--   It also copies the cryptol-to-rust compiler RTS into `target` meaning that
--   the resulting crate should be ready to compile with `cargo`.
writeExampleCrate :: String -> Rust.SourceFile () -> FilePath -> IO ()
writeExampleCrate crateName rust target =
  do  createDirectoryIfMissing True target
      writeFile (target </> "Cargo.toml") (crateToml crateName)
      writeRtsTo (target </> rtsPath)
      writeDwordTo (target </> dwordPath)

      let src = target </> "src"
      createDirectoryIfMissing True src

      writeFile (src </> "main.rs") $
        unlines
          [ show (RustPP.pretty' rust)
          , "pub fn main() {"
          , "  print!(\"{}\\n\",cry_main().display())"
          , "}"
          ]


