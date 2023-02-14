module Cryptol.Compiler.Interval where

import Data.Map(Map)
import Data.Map qualified as Map
import Control.Monad(foldM)

import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.InferTypes qualified as Cry
import Cryptol.TypeCheck.PP qualified as Cry
import Cryptol.TypeCheck.Solver.SMT qualified as Cry

import Cryptol.Compiler.PP

data Interval = IInfNat        -- ^ May finite or infinite
              | INat           -- ^ Any finite value
              | ISize          -- ^ Only finite values under a constant
                deriving Show

-- | Small bound is inclusive
schemaIntervals :: Integer -> Cry.Schema -> IO (Map Cry.TParam Interval)
schemaIntervals smallBound sch =
  Cry.withSolver (pure ()) cfg \solver ->
    Cry.examineProps solver (Cry.sVars sch) (Cry.sProps sch) \check ->
      foldM (getInterval smallBound check) Map.empty (Cry.sVars sch)
  where
  cfg = Cry.defaultSolverConfig [] -- { Cry.solverVerbose = 5 }


getInterval ::
  Integer ->
  ([Cry.Prop] -> IO Bool) ->
  Map Cry.TParam Interval -> Cry.TParam -> IO (Map Cry.TParam Interval)
getInterval smallBound check mp x
  | Cry.kindOf x == Cry.KNum =
    do sz <- do let v = Cry.TVar (Cry.TVBound x)
                notInf <- check [v Cry.=#= Cry.tInf]
                if notInf
                  then do notBig <- check
                                      [ v Cry.>== Cry.tNum (smallBound + 1) ]
                          pure (if notBig then ISize else INat)
                  else pure IInfNat

       pure (Map.insert x sz mp)

  | otherwise = pure mp

instance PP Interval where
  pp = showPP

ppSizeMap :: Cry.NameMap -> Map Cry.TParam Interval -> Doc
ppSizeMap ns mp =
  vcat [ pp (Cry.ppWithNames ns x) <+> ":" <+> pp s | (x,s) <- Map.toList mp ]
