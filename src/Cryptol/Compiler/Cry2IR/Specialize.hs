module Cryptol.Compiler.Cry2IR.Specialize
  ( compileFunDecl
  , compilePrimDecl
  , compileValType
  , compileStreamSizeType
  ) where

import Data.Map(Map)
import Data.Map qualified as Map
import Control.Applicative(Alternative(..))
import Control.Monad(forM_)

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.TypeCheck.Solver.Class qualified as Cry
import Cryptol.TypeCheck.Solver.Types qualified as Cry

import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.IR.EvalType

import Cryptol.Compiler.Cry2IR.Monad
import Cryptol.Compiler.Cry2IR.InstanceMap

compilePrimDecl :: Cry.Schema -> M.CryC [(FunInstance, FunType)]
compilePrimDecl s =
  do ans <- compileFunDecl (Cry.sVars s) (Cry.sProps s) args res k
     pure [ (a,b) | (a,b,_) <- ans ]
  where
  (args,res) = go [] (Cry.sType s)
  k _ _ = pure IRFunPrim

  go as ty =
    case ty of
      Cry.TUser _ _ ty1                 -> go as ty1
      Cry.TCon (Cry.TC Cry.TCFun) [a,b] -> go (a : as) b
      _                                 -> (reverse as, ty)

compileFunDecl ::
  (ApSubst a, TName a ~ Cry.TParam)  =>
  [Cry.TParam]                          {- ^ Type parameters -} ->
  [Cry.Prop]                            {- ^ Type qualifiers -} ->
  [Cry.Type]                            {- ^ Types of arguments -} ->
  Cry.Type                              {- ^ Type of result -} ->
  ([Type] -> Type -> SpecM a)           {- ^ Do actual work -} ->
  M.CryC [(FunInstance, FunType, a)]
compileFunDecl as quals args result k =
  case ctrProps (infoFromConstraints quals) of

    -- inconsistent
    Nothing -> pure []

    Just (traits,props,boolProps) ->
      runSpecM
        do addTParams as
           mapM_ addTrait traits
           addNumProps props
           forM_ (Map.toList boolProps) \(x,ps) ->
             case ps of
               [] -> addIsBoolProp x (Known False)
               _  -> addIsBoolProp x (Unknown ps)

           argTs <- mapM compileValType args
           resT  <- compileValType result
           body  <- k argTs resT
           (su,info) <- doTParams suEmpty [] as
           ts <- getTraits
           let tparams = [ x
                         | (x,ty) <- zip as info
                         , case ty of
                             TyNotBool -> True
                             TyAny     -> True
                             _         -> False
                         ]

           let sparams = [ (x, case s of
                                 MemSize   -> TSize
                                 LargeSize -> TInteger
                           )
                         | (x, NumVar s) <- zip as info ]



           pure ( FunInstance info
                , IRFunType
                    { ftTypeParams = tparams
                    , ftTraits     = ts
                    , ftSizeParams = sparams
                    , ftParams     = apSubst su argTs
                    , ftResult     = apSubst su resT
                    }
                , apSubst su body
                )
  where
  doTParams su info todo =
    case todo of
      [] -> pure (su, reverse info)
      x : xs
        | Cry.kindOf x == Cry.KNum ->
          do mbYes <- checkSingleValue x
             case mbYes of
               Just v ->
                 doTParams (suAddSize x sz su)
                           (NumFixed v : info)
                           xs
                 where sz = case v of
                              Cry.Inf -> IRInfSize
                              Cry.Nat n -> IRSize (IRFixedSize n)

               Nothing ->
                 do s <- caseSize (Cry.TVar (Cry.TVBound x))
                    case s of
                      IsFin     -> doTParams su (NumVar LargeSize : info) xs
                      IsFinSize -> doTParams su (NumVar MemSize : info) xs
                      IsInf     -> panic "compileFunType" ["IsInf"]

        | otherwise ->
          do i <- getBoolConstraint x
             case i of
               Known True -> doTParams (suAddType x TBool su) (TyBool : info) xs
               Known False -> doTParams su (TyNotBool : info) xs
               Unknown {} -> doTParams su (TyAny : info) xs





compileValType :: Cry.Type -> SpecM Type
compileValType ty =
  case ty of
    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum {}   -> unexpected "TCNum"
            Cry.TCInf      -> unexpected "TCInf"

            Cry.TCBit      -> pure TBool
            Cry.TCInteger  -> pure TInteger
            Cry.TCRational -> pure TRational

            Cry.TCFloat ->
              case ts of
                   [ e, p ] -> opt 8 24 TFloat <|> opt 11 53 TDouble
                     where
                     isK a n = a Cry.=#= Cry.tNum (n :: Int)
                     opt a b r =
                       do addNumProps [ isK e a, isK p b ]
                          pure  r
                   _ -> unexpected "Malformed TFCFloat"

            Cry.TCIntMod ->
              case ts of
                [ sz ] ->
                  do csz <- compileStreamSizeType sz
                     case csz of
                       IRSize s  -> pure (TIntegerMod s)
                       IRInfSize -> unexpected "TCIntMod Inf"
                _ -> unexpected "Malformed TCIntMod"


            Cry.TCSeq ->
              case ts of
                [tlen,tel] ->
                  do szf <- caseSize tlen
                     case szf of
                       IsInf -> TStream IRInfSize <$> compileValType tel
                       IsFin -> empty   -- too large
                       IsFinSize ->
                         do isize <- compileStreamSizeType tlen
                            vt    <- compileValType tel
                            case isize of
                              IRInfSize -> panic "InfSize when Fin" []
                              IRSize sz ->
                                case vt of
                                  TBool -> pure (TWord sz)
                                  TPoly x ->
                                    do yes <- caseBool x
                                       pure (if yes then TWord sz
                                                    else TArray sz vt)
                                  _  -> pure (TArray sz vt)

                _ -> unexpected "Malformed TSeq"

            Cry.TCTuple {}    -> TTuple <$> mapM compileValType ts

            Cry.TCFun         -> unsupported "higher order functions"
            Cry.TCArray       -> unsupported "Array type"
            Cry.TCAbstract {} -> unsupported "abstract value types"

        Cry.PC {}       -> unexpected "PC"
        Cry.TF {}       -> unexpected "TF"
        Cry.TError {}   -> unexpected "TError"

    Cry.TVar t ->
      case t of
        Cry.TVBound a -> pure (TPoly a)
        Cry.TVFree {} -> unexpected "TVFree"

    Cry.TUser _ _ t     -> compileValType t
    Cry.TRec {}         -> unsupported "records"    -- XXX
    Cry.TNewtype {}     -> unsupported "newtype"    -- XXX

  where
  unexpected msg = panic "compileValType" [msg]


-- | Compile a Cryptol size type to an IR type.
compileStreamSizeType :: Cry.Type -> SpecM StreamSize
compileStreamSizeType ty =

  case ty of
    Cry.TUser _ _ t -> compileStreamSizeType t

    Cry.TVar t ->
      case t of
        Cry.TVBound v ->
          do ctr <- caseSize ty
             case ctr of
               IsInf     -> pure IRInfSize
               IsFinSize -> pure (IRSize (IRPolySize MemSize v))
               IsFin     -> pure (IRSize (IRPolySize LargeSize v))
        Cry.TVFree {} -> unexpected "Free type variable"

    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum n       -> pure (IRSize (IRFixedSize n))
            Cry.TCInf         -> pure IRInfSize
            Cry.TCAbstract {} -> unsupported "abstract numeric types"

            Cry.TCBit         -> unexpected "TCBit"
            Cry.TCInteger     -> unexpected "TCInteger"
            Cry.TCFloat       -> unexpected "TCFloat"
            Cry.TCIntMod      -> unexpected "TCIntMod"
            Cry.TCRational    -> unexpected "TCRational"
            Cry.TCArray       -> unexpected "TCArray"
            Cry.TCSeq         -> unexpected "TCSeq"
            Cry.TCFun         -> unexpected "TCFun"
            Cry.TCTuple {}    -> unexpected "TCTuple"


        Cry.TF tf ->
          do isInf <- caseIsInf ty
             if isInf
                then pure IRInfSize
                else
                  do args <- mapM compileStreamSizeType ts
                     pure (evalSizeType tf args)

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNewtype {}     -> unexpected "TNewtype"
  where
  unexpected x = panic "compileStreamSizeType" [x]


--------------------------------------------------------------------------------
-- Information we learn from existing constraints

infoFromConstraints :: [Cry.Prop] -> ConstraintInfo
infoFromConstraints = foldr (CtrAnd . infoFromConstraint) CtrTrue

infoFromConstraint :: Cry.Prop -> ConstraintInfo
infoFromConstraint prop =
  case Cry.tNoUser prop of
    Cry.TCon (Cry.PC c) ts ->
      case c of
        Cry.PAnd             -> infoFromConstraints ts
        Cry.PTrue            -> CtrTrue

        Cry.PEqual           -> CtrProp prop
        Cry.PNeq             -> CtrProp prop
        Cry.PGeq             -> CtrProp prop
        Cry.PFin             -> CtrProp prop
        Cry.PPrime           -> CtrProp prop

        Cry.PHas {}          -> unexpected "PHas"

        Cry.PZero            -> trySolve1 (op 0) Cry.solveZeroInst PZero

        Cry.PLogic           -> trySolve1 (op 0) Cry.solveLogicInst PLogic

        Cry.PRing            -> notBool (op 0) `CtrAnd`
                                trySolve1 (op 0) Cry.solveRingInst  PRing
        Cry.PIntegral        -> notBool (op 0) `CtrAnd`
                                trySolve1 (op 0) Cry.solveIntegralInst PIntegral
        Cry.PField           -> notBool (op 0) `CtrAnd`
                                trySolve1 (op 0) Cry.solveFieldInst PField
        Cry.PRound           -> notBool (op 0) `CtrAnd`
                                trySolve1 (op 0) Cry.solveFieldInst PField

        Cry.PEq              -> trySolve1 (op 0) Cry.solveEqInst PEq

        Cry.PCmp             -> trySolve1 (op 0) Cry.solveCmpInst PCmp
        Cry.PSignedCmp       -> notBool (op 0) `CtrAnd`
                                trySolve1 (op 0) Cry.solveCmpInst PCmp

        Cry.PLiteral ->
          let n = op 0
              a = op 1
          in CtrProp (Cry.pFin n)
             `CtrAnd`
             ifBool a (CtrProp (Cry.tNum (1::Integer) Cry.>== n))
             `CtrAnd`
             trySolveLit n a Cry.solveLiteralInst PLiteral

        Cry.PLiteralLessThan ->
          ifBool (op 1) (CtrProp (Cry.tNum (2::Integer) Cry.>== op 0))
          `CtrAnd`
          trySolveLit (op 0) (op 1) Cry.solveLiteralLessThanInst PLiteral

        Cry.PFLiteral ->
          notBool (op 3)
          `CtrAnd`
          trySolve (op 3) PFLiteral
            (Cry.solveFLiteralInst (op 0) (op 1) (op 2) (op 3))

        Cry.PValidFloat {} -> CtrTrue -- XXX: restrict to supported ones?
      where
      op n =
        case splitAt n ts of
          (_,t:_) -> t
          _       -> unexpected "Expected 1 argument"

    _ -> CtrTrue

  where
  ifBool t k =
    case Cry.tNoUser t of
      Cry.TVar (Cry.TVBound x)      -> CtrIfBool x k
      Cry.TCon (Cry.TC Cry.TCBit) _ -> k
      _                             -> CtrTrue

  notBool t = ifBool t CtrFalse

  trySolve t nm res =
    case res of
      Cry.Unsolvable  -> CtrFalse
      Cry.SolvedIf xs -> infoFromConstraints xs
      Cry.Unsolved ->
        case Cry.tNoUser t of
          Cry.TVar (Cry.TVBound x) -> CtrTrait (IRTrait nm x)
          _ -> unexpected "Unsolved not TVar"

  trySolve1 t sol nm     = trySolve t nm (sol t)
  trySolveLit n a sol nm = trySolve a nm (sol n a)




  unexpected msg = panic "infoFromConstraint" [msg]


--------------------------------------------------------------------------------
data ConstraintInfo =
    CtrFalse
  | CtrTrue
  | CtrAnd ConstraintInfo ConstraintInfo
  | CtrProp Cry.Prop
  | CtrTrait Trait
  | CtrIfBool Cry.TParam ConstraintInfo
    -- ^ Currently we do not support arbitrary nested things here,
    -- see `ctrProps`.

-- | Returns unconditional assumptions, and ones that depend on the given
-- parameter being bool.  If the entry is `[]`, than the parameter must
-- not be bool.  If it we have some props, then they can be assumed but
-- only when the parameter *is* bool.
ctrProps ::
  ConstraintInfo -> Maybe ([Trait], [Cry.Prop], Map Cry.TParam [Cry.Prop])
ctrProps = go (mempty,mempty,mempty)
  where
  go props@(ts,pu,pc) ci =
    case ci of
      CtrFalse   -> Nothing
      CtrTrue    -> pure props
      CtrAnd x y ->
        do p1 <- go props x
           go p1 y

      CtrProp p -> pure (ts, p : pu, pc)

      CtrTrait t -> pure (t : ts, pu, pc)

      CtrIfBool x pr ->
        pure
          case pr of
            CtrFalse  -> (ts, pu, Map.insert x [] pc)
            CtrProp p ->
              case Map.lookup x pc of
                Just [] -> props
                _       -> (ts, pu, Map.insertWith (++) x [p] pc)
            _ -> panic "ctrProps" ["Unexpected nested prop"]



