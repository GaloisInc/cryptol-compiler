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
import MonadLib

import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Cry2IR.RecursiveStreams
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.Cry2IR.Monad qualified as S

type Seq = IRSeq Cry.TParam NameId Expr


compileRecSeqs :: [(Name, Seq)] -> S.SpecM Expr -> S.SpecM Expr
compileRecSeqs ds _k = S.unsupported (dbg ds)
  where
  dbg = Text.pack . show . vcat . map ppDef
  ppDef (x,d) = pp x <+> "=" <+> pp d

newtype StreamM a = StreamM (StateT RW S.SpecM a)
  deriving (Functor,Applicative,Monad)

newtype RW = RW
  { rwExterns :: [(Name, Expr)]   -- ^ Names for external streams
  }

doSpec :: S.SpecM a -> StreamM a
doSpec m = StreamM (lift m)

doCryC :: M.CryC a -> StreamM a
doCryC m = doSpec (S.doCryC m)

newExtern :: Expr -> StreamM Expr
newExtern e =
  do x <- doCryC M.newNameId
     let ty = typeOf e
         elTy = case ty of
                  TStream _ t -> t
                  _ -> panic "newExtern" ["Not a stream"]
     let name = IRName { irNameName = x, irNameType = ty }
     StreamM $ sets_ \rw -> rw { rwExterns = (name, e) : rwExterns rw }
     pure (callPrim Head [ IRExpr (IRVar name) ] elTy)



--------------------------------------------------------------------------------

-- XXX: we assume that if `ps` is extrenal, the `drop n ps` is also
-- external:
-- xs = [0] # [ (a,b) | a <- drop 1 ps, b <- xs ]
--
-- xs = [0,1] # drop 1 (if p then xs else qs)


nextFun ::
  Size {- ^ Current index -} ->
  Seq ->
  StreamM Expr

nextFun i s =
  case irSeqShape s of

    SeqExternal e -> newExtern e
    SeqVar x -> undefined

    SeqAppend xs ys ->
      do let frontLen = streamSizeToSize (irSeqLen s)
             cond  = callSizePrim LtSize [(i,MemSize), (frontLen,MemSize)] TBool
             j     = streamSizeToSize
                        (evalSizeType Cry.TCSub [ IRSize i, irSeqLen s ])
         front <- nextFun i xs
         back  <- nextFun j ys
         pure (IRExpr (IRIf cond front back))

    SeqDrop n xs ->
      -- XXX: Overflow?
      do let j = streamSizeToSize
                              (evalSizeType Cry.TCAdd [ IRSize i, IRSize n ])
         nextFun j xs

    SeqIf e xs ys ->
      do iThen <- nextFun i xs
         iElse <- nextFun i ys
         pure (IRExpr (IRIf e iThen iElse))

    SeqPar e gs -> undefined

    SeqSeq e gs -> undefined


{-

zig = [0] # zag
zag = [1] # zig

zig1 = [0,1] # zag1
zag1 = drop`{1} zig1

xs = [0,1] # ys


-}


