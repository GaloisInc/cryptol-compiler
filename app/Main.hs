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
import Cryptol.Parser.Position qualified as Cry

import Cryptol.IR.Eta(etaModule)

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.Cry2IR.InstanceMap(instanceMapToList)
import Cryptol.Compiler.Cry2IR.Compile
import Cryptol.Compiler.Cry2IR.Monad qualified as S
import Cryptol.Compiler.Cry2IR.Specialize qualified as S
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
              doSimpleCompile opts

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

doSimpleCompile :: Options -> CryC ()
doSimpleCompile opts =
  do ms <- getLoadedModules
     forM_ ms \m ->
        do let nm = show (cryPP (Cry.mName m))
           doIO (putStrLn ("Processing module: " ++ nm))
           doIO (putStrLn "Eta Expansion")
           tys <- getTypes
           m' <- doNameGen (\s -> etaModule tys s m)
           -- doIO (print (cryPP m'))
           doIO (putStrLn "Converting to IR")
           -- doIO (print (cryPP m'))
           compileModule m'

     decls <- getCompiled
     let declList = concatMap instanceMapToList (Map.elems decls)
         gi = GenInfo { genCurModule       = Cry.mName (last ms)
                      , genExternalModules = mempty
                      }

     srcFile <- doIO $ genModule gi declList

     let crateName = optCrateName opts
         outputPath = optOutputPath opts </> crateName
     doIO (saveExample crateName outputPath srcFile)

saveExample :: String -> FilePath -> Rust.SourceFile () -> IO ()
saveExample name dir rust = Crate.writeExampleCrate name rust dir


doTest :: CryC ()
doTest =
  do let x = tp 1
         y = tp 2
     let t = Cry.tMul (v x) (v y)
     rs <- S.runSpecM $
            do S.addTParams [x,y]
               S.compileSeqLenSizeType t
     doIO (putStrLn ("Result number: " ++ show (length rs)))
     doIO $ mapM_ (\x -> print (pp x) >> putStrLn "------") rs
  where
  v = Cry.TVar . Cry.tpVar
  tp n =
    Cry.TParam
      { Cry.tpUnique = n
      , Cry.tpKind = Cry.KNum
      , Cry.tpFlav = Cry.TPUnifyVar
      , Cry.tpInfo =
          Cry.TVarInfo
            { Cry.tvarSource = Cry.emptyRange
            , Cry.tvarDesc   = Cry.LenOfSeq
            }
      }

