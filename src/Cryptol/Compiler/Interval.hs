module Cryptol.Compiler.Interval where

import Data.Maybe(mapMaybe)
import Data.Map(Map)
import Data.Map qualified as Map
import Control.Monad(foldM)

import Cryptol.Utils.RecordMap qualified as Cry
import Cryptol.TypeCheck.Subst qualified as Cry (listParamSubst,apSubst)
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.PP qualified as Cry
import Cryptol.TypeCheck.Solver.SMT qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Monad

data Interval = Interval
  { canBeInfinite :: Bool
  , canBeLarge    :: Bool
  } deriving Show

{- | Compute rough bounds on numeric variables.
We consider the constraints in the schema, and we also assume that
the sizes of sequences are <= the size bound (if finite). -}
schemaIntervals ::
  Integer     {- ^ Values guaranteed to be <= this, will be ISize -} ->
  Cry.Schema  {- ^ Schem to analyze -} ->
  CryC (Map Cry.TParam Interval) {- ^ Ranges for numeric type parameters -}
schemaIntervals sizeBound sch =
  do solver <- getSolver
     doIO $ Cry.inNewFrame solver
       do tvars <- Cry.declareVars solver (map Cry.TVBound (Cry.sVars sch))
          let lens = seqLengths (Cry.sType sch)
                   $ seqLengthsMany (Cry.sProps sch) []
              lenPs = [ Cry.tNum sizeBound Cry.>== l | l <- lens ]
              numPs = mapMaybe toNumP (Cry.sProps sch)
          mapM_ (Cry.assume solver tvars) numPs
          foldM (getInterval solver tvars sizeBound lenPs)
                Map.empty
                (Cry.sVars sch)
  where
  toNumP p =
    case p of
      Cry.TUser _ _ t -> toNumP t
      Cry.TCon (Cry.PC Cry.PLiteral) [val,_] -> Just (Cry.pFin val)
      _ | Cry.isNumeric p -> Just p
        | otherwise -> Nothing

seqLengthsMany :: [Cry.Type] -> [Cry.Type] -> [Cry.Type]
seqLengthsMany xs done = foldr seqLengths done xs

-- | Find the numeric types that are lenghts of sequences
seqLengths :: Cry.Type -> [Cry.Type] -> [Cry.Type]
seqLengths ty done =
  case ty of
    Cry.TCon tcon ts  ->
      case tcon of
        Cry.TC tc     ->
          case tc of
            Cry.TCSeq     -> head ts : seqLengthsMany (tail ts) done
            _             -> seqLengthsMany ts done

        Cry.PC {}         -> seqLengthsMany ts done
        Cry.TF {}         -> done
        Cry.TError {}     -> done

    Cry.TVar {}     -> done
    Cry.TUser _ _ t -> seqLengths t done
    Cry.TRec rm     -> seqLengthsMany (Cry.recordElements rm) done

    Cry.TNewtype nt ts ->
      let su   = Cry.listParamSubst (zip (Cry.ntParams nt) ts)
          todo = map (Cry.apSubst su) (Cry.ntConstraints nt) ++
                 map (Cry.apSubst su) (Cry.recordElements (Cry.ntFields nt))
      in seqLengthsMany todo done



getInterval ::
  Cry.Solver ->
  Cry.TVars ->
  Integer ->
  [Cry.Prop] ->
  Map Cry.TParam Interval ->
  Cry.TParam ->
  IO (Map Cry.TParam Interval)
getInterval solver tvars sizeBound lenPs mp x
  | Cry.kindOf x == Cry.KNum =
    do let v = Cry.TVar (Cry.TVBound x)
       notInf <- Cry.unsolvable solver tvars [v Cry.=#= Cry.tInf]
       notBig <- Cry.inNewFrame solver
                 do Cry.assume solver tvars (Cry.pFin v)
                    mapM_ (Cry.assume solver tvars) lenPs
                    Cry.unsolvable solver tvars
                                  [v Cry.>== Cry.tNum (sizeBound + 1)]
       pure $ Map.insert x Interval { canBeInfinite = not notInf
                                    , canBeLarge    = not notBig
                                    } mp
  | otherwise = pure mp

instance PP Interval where
  pp = showPP

ppSizeMap :: Cry.NameMap -> Map Cry.TParam Interval -> Doc
ppSizeMap ns mp =
  vcat [ pp (Cry.ppWithNames ns x) <+> ":" <+> pp s | (x,s) <- Map.toList mp ]
