
-- | Utilities for generating crates and including the generated runtime libraries
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Cryptol.Compiler.Rust.Crate
  ( writeExampleCrate
  , mkCrate
  ) where

import Data.FileEmbed qualified as Embed
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Monad(forM_,unless)

import Language.Rust.Pretty qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Names(cryptolCrateString)

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
  String        {- ^ Name of the crate -} ->
  FilePath      {- ^ Directory to store the crate in -} ->
  [Rust.Ident]  {- ^ Crate `lib.rs` with these modules, if non-empty -} ->
  IO ()
mkCrate crateName target mods =
  do  createDirectoryIfMissing True target
      writeFile (target </> "Cargo.toml") (crateToml crateName)
      writeRtsTo (target </> rtsPath)
      writeDwordTo (target </> dwordPath)

      let src = target </> "src"
      createDirectoryIfMissing True src

      unless (null mods)
        do let lib = sourceFile Nothing (map pubMod mods)
           writeFile (src </> "lib.rs") (show (Rust.pretty' lib))



-- | `writeExampleCrate crateName targetDir sourceFile`  writes a single rust
--   source file to a new crate named `crateName` located at `targetDir` and adds a
--   `fn main` to invoke the `cry_main` function and print the result.
--
--   It also copies the cryptol-to-rust compiler RTS into `target` meaning that
--   the resulting crate should be ready to compile with `cargo`.
writeExampleCrate :: String -> FilePath -> RustExpr -> IO ()
writeExampleCrate crateName target expr =
  do  createDirectoryIfMissing True target
      writeFile (target </> "Cargo.toml") (crateToml crateName)
      writeRtsTo (target </> rtsPath)
      writeDwordTo (target </> dwordPath)

      let src = target </> "src"
      createDirectoryIfMissing True src

      let txt = show (Rust.pretty' expr)

      writeFile (src </> "main.rs") $
        unlines
            -- show (RustPP.pretty' rust)
          [ "pub fn main() {"
          , "  print!(\"{}\\n\",(" ++ txt ++ "))"
          , "}"
          ]


