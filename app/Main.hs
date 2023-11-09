module Main where

import Control.Exception
import Control.Monad(forM)
import System.IO(hPrint,stderr)
import System.Exit
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List(intersperse)
import Data.Maybe(mapMaybe)

import Cryptol.TypeCheck.AST qualified  as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.Parser.Position qualified as Cry

import Cryptol.Compiler.Error
import Cryptol.Compiler.Monad
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.Cry2IR.SpecializeM qualified as S
import Cryptol.Compiler.Cry2IR.Specialize qualified as S

import Cryptol.Compiler.Driver(loadInputs, cry2rust)


import Options

main :: IO ()
main =
  do opts <- getOptions
     runCryC (optOutputPath opts) (optCrateName opts)
       case optCommand opts of

         ShowHelp ->
           doIO showHelp

         DefaultCommand ->
            cry2rust (optFiles opts)
{-
           do Compiler.loadInputs (optFiles opts)
              void $ Compiler.doSimpleCompile (optCrateName opts) (optOutputPath opts)
-}

         ListPrimitives ->
           do loadInputs (optFiles opts)
              listPrimitives

  `catch` \e ->
    do hPrint stderr (pp (e :: CompilerError))
       exitFailure



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

doTest :: CryC ()
doTest =
  do let x = tp 1
         y = tp 2
     let t = Cry.tMul (v x) (v y)
     rs <- S.runSpecM $
            do S.addTParams [x,y]
               S.compileSeqLenSizeType t
     doIO (putStrLn ("Result number: " ++ show (length rs)))
     doIO $ mapM_ (\i -> print (pp i) >> putStrLn "------") rs
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

