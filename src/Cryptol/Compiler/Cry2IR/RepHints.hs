module Cryptol.Compiler.Cry2IR.RepHints where

import Data.Map(Map)
import Data.Map qualified as Map

import Cryptol.Utils.RecordMap qualified as Cry
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.PP

data RepHint = AsWord
             | AsArray RepHint
             | AsStream RepHint
             | NoHint
             | RepHint :-> RepHint
             | TupHint [RepHint]
             | RecHint (Cry.RecordMap Cry.Ident RepHint)

-- | Split into hints for the arguments and a hint for the result
splitRepHint :: RepHint -> ([RepHint], RepHint)
splitRepHint hint =
  case hint of
    x :-> b ->
      let (xs,y) = splitRepHint b
      in (x:xs,y)
    _ -> ([],hint)


infixr 1 :->

-- 0: don't wrap
-- 1: wrap functions
-- 2: wrap applications
instance PP RepHint where
  pp hint =
    case hint of
      NoHint      -> "_"
      AsWord      -> "Word"
      AsArray h   -> parensAfter 1 ("Array" <+> withPrec 2 (pp h))
      AsStream h  -> parensAfter 1 ("Stream" <+> withPrec 2 (pp h))
      x :-> y     ->
        parensAfter 0 (withPrec 1 (pp x) <+> "->" <+> withPrec 0 (pp y))
      TupHint hs -> withPrec 0 (parens (commaSep (map pp hs)))
      RecHint r ->
        withPrec 0 (braces (commaSep (map ppF (Cry.displayFields r))))
        where ppF (x,y) = cryPP x <+> "=" <+> pp y

primRepHints :: Map Cry.PrimIdent [RepHint]
primRepHints = Map.fromList
  [ (Cry.prelPrim "fromTo", [ AsStream NoHint ])
  , (Cry.prelPrim "fromToLessThan", [ AsStream NoHint ])
  , (Cry.prelPrim "fromToBy", [ AsStream NoHint ])
  , (Cry.prelPrim "fromToByLessThan", [ AsStream NoHint ])
  , (Cry.prelPrim "fromToDownByGreaterThan", [ AsStream NoHint ])
  , (Cry.prelPrim "fromThenTo", [ AsStream NoHint ])

  , (Cry.prelPrim "#", [ AsWord :-> AsWord :-> AsWord
                       , AsStream NoHint :-> AsStream NoHint :-> AsStream NoHint
                       ])

  , (Cry.prelPrim "zip",
        [ AsStream NoHint :-> AsStream NoHint :-> AsStream NoHint ])

  , (Cry.prelPrim "zipWith",
        [ NoHint :-> AsStream NoHint :-> AsStream NoHint :-> AsStream NoHint ])
  ]

