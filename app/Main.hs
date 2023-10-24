module Main where

import Control.Exception
import Control.Monad(forM_,forM)
import System.IO(hPrint,stderr)
import System.Exit
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List(intersperse)
import Data.Maybe(mapMaybe)
import System.Directory(createDirectoryIfMissing)
import System.FilePath (splitDirectories, (</>), joinPath)

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Pretty qualified as Rust

import Cryptol.TypeCheck.AST qualified  as Cry
import Cryptol.ModuleSystem.Name qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.Cry2IR.InstanceMap(instanceMapToList)
import Cryptol.Compiler.Cry2IR.Compile
import Cryptol.Compiler.Rust.CodeGen
import Cryptol.Compiler.Rust.Crate qualified as Crate


import Options


main :: IO ()
main =
  do opts <- getOptions
     runCryC
       case optCommand opts of

         ShowHelp ->
           doIO showHelp

         DefaultCommand ->
           do loadInputs opts
              doSimpleCompile

         ListPrimitives ->
           do loadInputs opts
              listPrimitives

  `catch` \e ->
    do hPrint stderr (pp (e :: CompilerError))
       exitFailure

loadInputs :: Options -> CryC ()
loadInputs opts =
  case optFiles opts of
    [] -> mapM_ loadModuleByName [ floatName, preludeName ]
    _  -> mapM_ loadModuleByPath (optFiles opts)

listPrimitives :: CryC ()
listPrimitives =
  do ps <- listPrims
     grouped <- forM ps \pname ->
                  do s <- getSchemaOf (Cry.EVar pname)
                     pure (schemaClass s, [(pname,s)])
     doIO $ print
          $ vcat $ map ppClass
          $ Map.toList
          $ Map.fromListWith (++) grouped
  where
  schemaClass s = Set.fromList (mapMaybe isClass (Cry.sProps s))

  ppClass (nm,es)
    | Set.null nm = methods
    | otherwise = hsep ("trait" : intersperse "+" (map pp (Set.toList nm)))
                    $$ nest 2 methods
    where methods = vcat [ fsep [pp x <.> ":", cryPP s] | (x,s) <- es ]

  isClass ty =
    case Cry.tNoUser ty of
      Cry.TCon (Cry.PC pc) _ ->
        case pc of
          Cry.PZero             -> Just PZero
          Cry.PLogic            -> Just PLogic
          Cry.PRing             -> Just PRing
          Cry.PIntegral         -> Just PIntegral
          Cry.PField            -> Just PField
          Cry.PRound            -> Just PRound
          Cry.PEq               -> Just PEq
          Cry.PCmp              -> Just PCmp
          Cry.PSignedCmp        -> Just PSignedCmp
          Cry.PLiteral          -> Just PLiteral
          Cry.PLiteralLessThan  -> Just PLiteral
          Cry.PFLiteral         -> Just PFLiteral
          _                     -> Nothing

      _ -> Nothing

isPrel :: Cry.Name -> Bool
isPrel x = Cry.nameTopModule x `elem` [preludeName, floatName]

doSimpleCompile :: CryC ()
doSimpleCompile =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do doIO (putStrLn ("Converting to IR: " ++ show (cryPP (Cry.mName m))))
           -- doIO (print (cryPP m))
           compileModule m

     decls <- getCompiled
     let declList = concatMap instanceMapToList (Map.elems decls)
         gi = GenInfo { genCurModule       = Cry.mName (last ms)
                      , genExternalModules = mempty
                      }

     srcFile <- doIO $ genModule gi declList
     doIO (saveExample "example" srcFile)
     -- doIO (print (Rust.pretty' srcFile))

saveExample :: FilePath -> Rust.SourceFile () -> IO ()
saveExample dir rust = Crate.writeExampleCrate "test-program" rust dir




