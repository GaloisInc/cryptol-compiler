{-|
Lift local functions to the top level.
  1. Free local variables become parameters
  2. Any type variables that are mentioned become type parameters
  3. Any constraints that mention these variables.
  3.5 Note that if these constraints mention variables that are not mentioned
      anywhere else we need to also add *them*, which in turn might add more
      constraints, etc.
  4. Each use of the local function needs to pass these additional
     parameters.
  5. Note that locally defined functions (i.e., non-parameter ones)
     do not become paramters because they will be lifter.

In summary, given: `e where ds` and the `ds` are in dependency order.
  For each `d` in order:
    * rewrite definition to eliminte any functions that were already lifted.
    * compute free variables (types, values, predicates)
    * using this add a new top-level declaration.
    * remember how to change the instantiations, in the continuation
  Once we've done all the `ds` we need to rewrite the `e`

For simplicity, we assume that we have mono local bindings on,
which means that local functions will always be monomorphic
(they might still refer to type variables in their parent though)

f xs  ~>   f @freeTs @propConstraints (freeVars ++ xs)


-}
module Cryptol.IR.LambdaLift where

import Cryptol.IR.FreeVars




