module Cryptol.Compiler.Cry2IR.RecursiveStreams where

import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.IR.Cryptol


-- | A sequence, annotate with its length.
data IRSeq tname name e = IRSeq
  { irSeqLen    :: IRStreamSize tname
  , irSeqShape  :: IRSeqShape tname name e
  }

-- | The language for defining recursive sequence equations
data IRSeqShape tname name e =
    SeqExternal e
    -- ^ An external sequence
    -- (maybe not depend on variables from this recursive group)

  | SeqVar (IRName tname name)
    -- ^ A sequence from this recursive group

  | SeqAppend (IRSeq tname name e) (IRSeq tname name e)
    -- ^ Append sequences

  | SeqIf e (IRSeq tname name e) (IRSeq tname name e)
    -- ^ Conditional sequence

  | SeqDrop (IRSize tname) (IRSeq tname name e)
    -- ^ Drop elements from a sequence

  | SeqPar e [ (IRName tname name, IRSeq tname name e) ]
    -- ^ Parallel comprehension, single generator in each arm.
    -- @[ e | a <- xs | b <- ys ]@

  | SeqSeq e [ (IRName tname name, IRSeq tname name e) ]
    -- ^ Sequential comprehension (at least 2 entries in the list)
    -- @[ e | a <- xs, b <- ys ]@

seqSeq ::
  IRStreamSize tname ->
  e ->
  [ (IRName tname name, IRSeq tname name e) ] ->
  IRSeq tname name e
seqSeq len e ms =
  IRSeq
    { irSeqLen = len
    , irSeqShape =
        case ms of
          []  -> panic "seqSeq" ["[]"]  -- This could become the empty list,
                                        -- but it really shouldn't happen
          [_] -> SeqPar e ms
          _   -> SeqSeq e ms
    }

seqSeq' ::
  Eq tname =>
  [IRStreamSize tname] {- ^ lengths of matches -} ->
  e ->
  [ (IRName tname name, IRSeq tname name e) ] ->
  IRSeq tname name e
seqSeq' lens = seqSeq (newLen lens)
  where
  newLen todo =
    case todo of
      [] -> K 0
      [l] -> l
      l : more -> evalSizeType Cry.TCMul [l,newLen more]



instance (PP tname, PP name, PP e) => PP (IRSeq tname name e) where
  pp = pp . irSeqShape

instance (PP tname, PP name, PP e) => PP (IRSeqShape tname name e) where
  pp se =
    case se of
     SeqExternal e -> "EXT" <.> parens (withPrec 0 (pp e))
     SeqVar x -> pp x
     SeqAppend xs ys -> vcat [ pp xs, "#", pp ys ]
     SeqIf e s1 s2 ->
       parensAfter 0 $
       withPrec 0 $
       vcat [ "if" <+> pp e
            , nest 2 "then" <+> pp s1
            , nest 2 "else" <+> pp s2
            ]

     SeqDrop n s -> "drop" <.> parens (withPrec 0 (commaSep [pp n, pp s]))
     SeqPar e ps -> ppC "|" e ps
     SeqSeq e ps -> ppC "," e ps

    where
    ppM (x,e) = pp x <+> "<-" <+> pp e
    ppC sep e ps =
      let prefs = "|" : repeat sep
      in withPrec 0
           (vcat (("[" <+> pp e) : zipWith (<+>) prefs (map ppM ps) ++ ["]"]))

--------------------------------------------------------------------------------
