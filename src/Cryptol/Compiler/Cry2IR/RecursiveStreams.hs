{- |

The `i`-th element of a sequence may be defined used the following equations:

    (a # b) @ i
      | i < |a|     = a @ i
      | otherwise   = b @ (i - |a|)

    drop`{n} a @ i  = a @ (i + n)

    [a,b,c] @ i
      | i == 0 = a
      | i == 1 = b
      | i == 2 = c

    [ e | a <- xs, b <- ys ] @ i =
      let a = xs @ i
          b = ys @ i
      in e

    [ e | a <- xs, b <- ys, c <- zs ] @ i =
      let (xy_i,z_i) = divMod i    |zs|
          (x_i, y_i) = divMod xy_i |ys|
          a = xs @ x_i
          b = ys @ y_i
          c = zs @ z_i
      in e

    reverse xs @ i = xs @ (|xs| - 1 - i)
-}
module Cryptol.Compiler.Cry2IR.RecursiveStreams where

import Cryptol.Compiler.IR


-- | The language for defining recursive sequence equations
data Seq tname name e =
    SeqLiteral [ e ]
    -- ^ A literal sequence

  | SeqExternal e
    -- ^ An external sequence
    -- (maybe not depend on variables from this recursive group)

  | SeqVar (IRName tname name)
    -- ^ A sequence from this recursive group

  | SeqAppend [ Seq tname name e ]
    -- ^ Append sequences

  | SeqDrop (IRSize tname) (Seq tname name e)
    -- ^ Drop elements from a sequence

  | SeqPar e [ (IRName tname name, Seq tname name e) ]
    -- ^ Parallel comprehension, single generator in each arm

  | SeqSeq e [ (IRName tname name, Seq tname name e) ]
    -- ^ Sequential comprehension (at least 2 entries in the list)


{-

x = [1,2] # y
y = drop`{1} x

x @ i
  | i < |init|  = init @ i
  | otherwise   = y @ (i - |init|)

y @ i = x @ (i + 1)


y @ 0 -> x @ 1 -> []
y @ 1 -> x @ 2 -> []
y @ 2 -> x @ 3 -> y @ 1 -> []

-}




