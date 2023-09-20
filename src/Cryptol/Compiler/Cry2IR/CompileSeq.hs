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
module Cryptol.Compiler.Cry2IR.CompileSeq where

import qualified Data.Text as Text
import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Cry2IR.RecursiveStreams
import Cryptol.Compiler.Cry2IR.Monad

type Seq = IRSeq Cry.TParam NameId Expr


compileRecSeqs :: [(Name, Seq)] -> SpecM Expr -> SpecM Expr
compileRecSeqs ds _k = unsupported (dbg ds)
  where
  dbg = Text.pack . show . vcat . map ppDef
  ppDef (x,d) = pp x <+> "=" <+> pp d


{-



xs = [0,1] # xs

init() {
  extern1 = [0.1].into_iter();
  hist_x = [udnef,undef];
  i = 0;
}

next() {
  let a = if i < 2
            then extern1.next()
            else hist_x[i%2]
  hist_x[i % 2] = a
  i += 1
  a
}

--------------------------------------------------------------------------------

fibs = [0,1] # [ x + y  | x <- fibs | y <- drop`{1} fibs ]

init():
  extren1 = [0,1].into_iter()
  hist_fibs = [ undef, undef ]
  i = 0

next():
  fibs = if i < 2
           extern1.next()
         else
           let x = hist_fibs[(i-2) % 2]
           let y = hist_fibs[(i-2+1) % 2]
  hist[i % 2] = fibs
  fibs

{ i = 0, hist_fibs=[u,u], extern = [0,1] }
next()
  fibs = 0; extern = [1]; hist[0] = 0     -> 0
{ i = 1, hist_fibs=[0,u], exten = [1] }
next()
  fibs = 1; extern = []; hist[1] = 1      -> 1
{i = 2, hist_fibs=[0,1], externs = [] }
next()
  x = hist[0] = 0
  y = hist[1] = 1
  fibs = 0 + 1 = 1
  hist[0] = 1
{ i = 3, hist_fibs=[1,1], extern = [] } -> 1
next()
  x = hist[1] = 1
  y = hist[2 % 2] = hist[0] = 1
  fib = 1 + 1 = 2
  hist[3 % 2] = hist[1] = 2   -> 2
{ i = 4, hist = [1,2] }
next()
  x = hist[2 % 2] = hist[0] = 1
  y = hist[3 % 2] = hist[1] = 2
  fibs = 3
  hist[2] = 2\


--------------------------------------------------------------------------------

Hmm:
xss = [[0]] # [ [x+1 | x <- xs ] | xs <- xss ]




--------------------------------------------------------------------------------




zig = [0] # zag
zag = [1] # zig
xs  = [ (a,b,c,d) | a <- ps | b <- zig | c <- zag | d <- ps ]

init() {
  extern1 = ps.into_iter();
  extern4 = ps.into_iter();   -- 2 separate uses of `ps`
  extern2 = [0].into_iter():
  extern3 = [1].into_iter();
  hist_zig = [undef]
  hist_zag = [undef]
  i = 0;
}

next() {
  zig = if i < 1
          then extern2.next()
          else hist_zag[i % 1]
  zag = if i< 1 then extern3.next() else hist_zig[i % 1]
}




 -}

nextFun ::
  Size {- ^ Current index -} ->
  Seq ->
  SpecM Expr

nextFun i s =
  case irSeqShape s of

    SeqExternal e -> undefined
    SeqVar x -> undefined

    SeqAppend xs ys ->
      do let frontLen = streamSizeToSize (irSeqLen s)
             cond  = callSizePrim LtSize [(i,MemSize), (frontLen,MemSize)] TBool
             j     = streamSizeToSize
                        (evalSizeType Cry.TCSub [ IRSize i, irSeqLen s ])
         front <- nextFun i xs
         back  <- nextFun j ys
         pure (IRExpr (IRIf cond front back))

    SeqDrop n xs -> undefined

    SeqIf e xs ys ->
      do iThen <- nextFun i xs
         iElse <- nextFun i ys
         pure (IRExpr (IRIf e iThen iElse))

    SeqPar e gs -> undefined

    SeqSeq e gs -> undefined



