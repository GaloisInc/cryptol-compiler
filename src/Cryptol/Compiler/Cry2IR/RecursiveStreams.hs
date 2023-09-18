{- |

The `i`-th element of a sequence may be defined used the following equations:
@
    (a # b) \@ i
      | i < |a|     = a \@ i
      | otherwise   = b \@ (i - |a|)

    drop`{n} a \@ i  = a \@ (i + n)

    take`{n} a \@ i  = a \@ i

    // assumes `p`, `q`, `a` not in other alternative
    [ e | let p = pe, a <- xs, let q = pq | b <- ys ] \@ i =
      let p = pe
          a = xs \@ i
          q = pq
          b = ys \@ i
      in e

    [ e | a <- xs, b <- ys, c <- zs ] \@ i =
      let (xy_i,z_i) = divMod i    |zs|
          (x_i, y_i) = divMod xy_i |ys|
          a = xs \@ x_i
          b = ys \@ y_i
          c = zs \@ z_i
      in e

    (if e then s1 else s2) @ i = if e then s1 @ i else s2 @ i

    (e where ds) @ i           = (e @ i) where ds
@
-}
module Cryptol.Compiler.Cry2IR.RecursiveStreams where

import Data.List(intersperse)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol


-- | The language for defining recursive sequence equations
data IRSeq tname name e =
    SeqExternal (IRStreamSize tname) e
    -- ^ An external sequence
    -- (maybe not depend on variables from this recursive group)

  | SeqVar (IRName tname name)
    -- ^ A sequence from this recursive group

  | SeqAppend [ IRSeq tname name e ]
    -- ^ Append sequences

  | SeqIf e (IRSeq tname name e) (IRSeq tname name e)
    -- ^ Conditional sequence

  | SeqDrop (IRSize tname) (IRSeq tname name e)
    -- ^ Drop elements from a sequence

  | SeqPar e [ (IRName tname name, IRSeq tname name e) ]
    -- ^ Parallel comprehension, single generator in each arm
    -- @[ e | a <- xs | b <- ys ]@

  | SeqSeq e [ (IRName tname name, IRSeq tname name e) ]
    -- ^ Sequential comprehension (at least 2 entries in the list)
    -- @[ e | a <- xs, b <- ys ]@


instance (PP tname, PP name, PP e) => PP (IRSeq tname name e) where
  pp se =
    case se of
     SeqExternal _ e -> "EXT" <.> parens (withPrec 0 (pp e))
     SeqVar x -> pp x
     SeqAppend xs -> fsep (intersperse "#" (map pp xs))
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

