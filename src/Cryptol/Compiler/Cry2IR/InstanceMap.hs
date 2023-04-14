module Cryptol.Compiler.Cry2IR.InstanceMap
  ( ParamInfo(..)
  , FunInstance(..)
  , InstanceMap
  , singletonInstanceMap
  , mergeInstanceMap
  , instanceMapFromList
  , lookupInstance
  , ITE(..)
  ) where

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.Common
import Cryptol.Compiler.IR.EvalType


-- | A map where the keys are function instances.
data InstanceMap a =
    InstanceMap [ (ParamInfo, InstanceMap a) ]
  | Result a

instanceMapFromList :: PP a => [(FunInstance,a)] -> InstanceMap a
instanceMapFromList ins =
  case ins of
    [] -> panic "instanceMapFromList" ["empty list"]
    _  -> foldr1 mergeInstanceMap (map (uncurry singletonInstanceMap) ins)

-- | A singleton map associating the given instance with the value.
singletonInstanceMap :: FunInstance -> a -> InstanceMap a
singletonInstanceMap (FunInstance pis) a =
  case pis of
    []     -> Result a
    p : ps -> InstanceMap [ (p, singletonInstanceMap (FunInstance ps) a ) ]

-- | Merge two isntance maps.
-- More specific instances shadow less specific ones.
mergeInstanceMap :: PP a => InstanceMap a -> InstanceMap a -> InstanceMap a
mergeInstanceMap mp1 mp2 =
  case (mp1,mp2) of
    (InstanceMap opts1, InstanceMap opts2) ->
      InstanceMap (mergeOpts opts1 opts2)
    _ -> panic "mergeInstanceMap" ["Cannot merge"
                                  , show ("Left"  $$ nest 2 (pp mp1))
                                  , show ("Right" $$ nest 2 (pp mp2))
                                  ]

  where
  mergeOpts opts1 opts2 =
    case (opts1,opts2) of
      ([],_) -> opts2
      (_,[]) -> opts1
      ( (p1,k1) : more1, (p2,k2) : more2 ) ->
        case compare p1 p2 of
          EQ -> (p1, mergeInstanceMap k1 k2) : mergeOpts more1 more2
          LT -> (p1,k1) : mergeOpts more1 opts2
          GT -> (p2,k2) : mergeOpts opts1 more2


-- | Lookup some types in an instance map.
-- NOTE: This assumes that the instnace is already in the map!
-- Returns a decision procedure describing the matching instances.
lookupInstance ::
  [Either (IRType tname) (IRStreamSize tname)] ->
  InstanceMap a ->
  ITE tname a
lookupInstance ts mp = lookupInstance' ts mp NotFound

lookupInstance' ::
  [Either (IRType tname) (IRStreamSize tname)] ->
  InstanceMap a ->
  ITE tname a ->
  ITE tname a
lookupInstance' ts0 mp onFail =
  case mp of
    Result a -> Found a
    InstanceMap opts ->
      case ts0 of
        []     -> onFail
        t : ts -> search t ts opts

  where
  search t ts opts =
    case opts of
      [] -> onFail
      (p,k) : moreOpts ->
        let res = case t of
                    Left ty -> matchType p ty
                    Right s -> matchSize p s
            cont   = lookupInstance' ts k orElse
            orElse = search t ts moreOpts
         in case res of
              NoMatch -> search t ts moreOpts
              MatchIf [] -> cont
              MatchIf _
                | NotFound <- orElse -> cont
                  -- here we assume that instance is in the map

              -- we have a guard, and the alternative is not always failing
              MatchIf gs -> ITE gs cont orElse


-- | Result of looking up something in an instance map
data ITE tname a =
    ITE [Guard tname] (ITE tname a) (ITE tname a)
  | Found a
    -- ^ Unconditional match.
    -- Returns the value, together with an instantiations 

  | NotFound -- ^ Not found

data Guard tname = GBool tname
                 | GNotBool tname
                 | GNum (IRSizeName tname) Integer
                 | GNumFun Cry.TFun [IRStreamSize tname] Ordering Integer

data Match tname = NoMatch
                 | MatchIf [Guard tname]

matchType :: ParamInfo -> IRType tname -> Match tname
matchType parami ty =
  case parami of

    TyBool ->
      case ty of
        TBool   -> MatchIf []
        TPoly x -> MatchIf [ GBool x ]
        _       -> NoMatch

    TyNotBool ->
      case ty of
        TBool   -> NoMatch
        TPoly x -> MatchIf [GNotBool x]
        _       -> MatchIf []

    TyAny -> MatchIf []

    NumFixed {} -> unexpected "NumFixed"
    NumVar {}   -> unexpected "NumVar"
  where
  unexpected msg = panic "matchType" [msg]


matchSize :: ParamInfo -> IRStreamSize tname -> Match tname
matchSize parami ty =
  case parami of

    NumFixed x ->
      case ty of
        IRInfSize -> if x == Cry.Inf then MatchIf [] else NoMatch
        IRSize s ->
          case s of
            IRFixedSize n ->
              if x == Cry.Nat n then MatchIf [] else NoMatch
            IRPolySize v ->
              case x of
                Cry.Inf -> NoMatch
                Cry.Nat n ->
                  case irsSize v of
                    MemSize -> if n <= maxSizeVal
                                  then MatchIf [ GNum v n ]
                                  else NoMatch
                    LargeSize -> if n > maxSizeVal
                                   then MatchIf [ GNum v n ]
                                   else NoMatch

            IRComputedSize f ts ->
              case x of
                Cry.Inf   -> NoMatch
                Cry.Nat n ->
                  case sizeTypeSize s of
                    MemSize | n > maxSizeVal -> NoMatch
                    _ -> MatchIf [ GNumFun f ts EQ n ]

    NumVar x ->
      case ty of
        IRInfSize -> NoMatch
        IRSize sz ->
          case sz of
            IRFixedSize n ->
              case x of
                MemSize   -> if n <= maxSizeVal then MatchIf [] else NoMatch
                LargeSize -> if n > maxSizeVal then MatchIf [] else NoMatch
            IRPolySize v -> if x == irsSize v then MatchIf [] else NoMatch
            IRComputedSize f ts ->
              case (x,sizeTypeSize sz) of
                (MemSize,MemSize) -> MatchIf []
                (LargeSize,MemSize) -> NoMatch
                (MemSize,LargeSize) -> MatchIf [GNumFun f ts LT (maxSizeVal+1)]
                (LargeSize,LargeSize) -> MatchIf [GNumFun f ts GT maxSizeVal]

    TyBool {}     -> unexpected "TyBool"
    TyNotBool {}  -> unexpected "TyNotBool"
    TyAny {}      -> unexpected "TyAny"

  where
  unexpected msg = panic "matchSize" [msg]


--------------------------------------------------------------------------------
-- Pretty printing

instance PP tname => PP (Guard tname) where
  pp gu =
    case gu of
      GBool x    -> pp x <+> "==" <+> "Bool"
      GNotBool x -> pp x <+> "!=" <+> "Bool"
      GNum x i   -> pp x <+> "==" <+> pp i
      GNumFun f ts op i -> fun <+> pop <+> pp i
        where
        fun = cryPP f <+> withPrec 9 (hsep (map pp ts))
        pop = case op of
                EQ -> "=="
                LT -> "<"
                GT -> ">"


instance (PP tname, PP a) => PP (ITE tname a) where
  pp ite =
    case ite of
      NotFound   -> "NotFound"
      Found a    -> "Found" $$ nest 2 (pp a)
      ITE gs a b ->
        vcat [ "if" <+> parens (commaSep (map pp gs))
             , nest 2 "then" <+> pp a
             , nest 2 "else" <+> pp b
             ]

instance PP a => PP (InstanceMap a) where
  pp im =
    case im of
      Result a -> "Success" $$ nest 2 (pp a)
      InstanceMap opts ->
        case opts of
          [] -> "Empty"
          _  -> vcat [ (pp p <> ":") $$ nest 2 (pp k) | (p,k) <- opts ]
