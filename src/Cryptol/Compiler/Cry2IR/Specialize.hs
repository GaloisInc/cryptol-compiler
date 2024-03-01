module Cryptol.Compiler.Cry2IR.Specialize
  ( compileSchema, PropInfo(..)
  , compileValType
  , compileSizeType
  , compileStreamSizeType
  , compileSeqLenSizeType
  , compileStreamLenSizeType
  ) where

import Data.Map(Map)
import Data.Map qualified as Map
import Control.Monad(zipWithM,when)
import Control.Applicative(Alternative(..))

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.TypeCheck.Solver.Class qualified as Cry
import Cryptol.TypeCheck.Solver.Types qualified as Cry
import Cryptol.Utils.RecordMap qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.IR.EvalType

import Cryptol.Compiler.Cry2IR.SpecializeM
import Cryptol.Compiler.Cry2IR.RepHints

compileSchema ::
  Cry.Name                              {- ^ Name of what we are compiling -} ->
  Cry.Schema                            {- ^ Schema to compile -} ->
  M.CryC [(FunInstance, FunType, PropInfo)]
compileSchema cn sch =
  case ctrProps (infoFromConstraints (Cry.sProps sch)) of

    -- inconsistent
    -- XXX: should we say something here?
    Nothing -> pure []

    Just (traits,props,boolProps) ->
      runSpecM $
        enter cn
        do addTParams (Cry.sVars sch)
           mapM_ addTrait traits
           addNumProps props
           mapM_ (uncurry addIsBoolProp) (Map.toList boolProps)
           specTy <-
              do mbHint <- getRepHint cn
                 case mbHint of
                   Nothing   -> compileValType False (Cry.sType sch)
                   Just hint -> compileValTypeWithHint False hint (Cry.sType sch)

           let as = Cry.sVars sch
           (su,info) <- doTParams suEmpty [] as
           ts <- getTraits
           let tparams = [ x
                         | (x,ty) <- zip as info
                         , case ty of
                             TyNotBool -> True
                             TyAny     -> True
                             _         -> False
                         ]

           let sparams = [ IRSizeName x s
                         | (x, NumVar s) <- zip as info ]

           let (args,res) = case apSubst su specTy of
                              TFun x y -> (x, y)
                              y        -> ([], y)

           allPs <- getTParams
           allProps <- getProps

           pure ( FunInstance info
                , IRFunType
                    { ftTypeParams = tparams
                    , ftTraits     = ts
                    , ftSizeParams = sparams
                    , ftParams     = args
                    , ftResult     = res
                    }
                , PropInfo { propVars = allPs, propAsmps = allProps }
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
                      IsInf     -> doTParams su (NumFixed Cry.Inf : info) xs

        | otherwise ->
          do i <- getBoolConstraint x
             case i of
               Known True -> doTParams (suAddType x TBool su) (TyBool : info) xs
               Known False -> doTParams su (TyNotBool : info) xs
               Unknown {} -> doTParams su (TyAny : info) xs




compileValTypeWithHint ::
  Bool {- ^ Are we nested in an array? -} ->
  RepHint -> Cry.Type -> SpecM Type
compileValTypeWithHint nested hint ty =

  case hint of
    NoHint -> compileValType nested ty

    AsWord ->
      case Cry.tIsSeq ty of
        Just (lenTy,elTy) ->
          do case Cry.tIsVar elTy of
               Just (Cry.TVBound a) -> addIsBoolProp a (Known True)
               _ | Cry.tIsBit elTy -> pure ()
                 | otherwise       -> bad
             n <- compileSeqLenSizeType lenTy
             pure (TWord n)

        Nothing -> bad

    AsArray h ->
      case Cry.tIsSeq ty of
        Just (lenTy,elTy) ->
          do n <- compileSeqLenSizeType lenTy
             case Cry.tIsVar elTy of
               Just (Cry.TVBound a) -> addIsBoolProp a (Known False)
               _ -> when (Cry.tIsBit elTy) bad

             a <- compileValTypeWithHint True h elTy
             pure (TArray n a)

        Nothing -> bad

    AsStream h
      | nested -> bad
      | otherwise ->
        case Cry.tIsSeq ty of
          Just (l, elTy) ->
            do len <- compileStreamLenSizeType l
               t   <- compileValTypeWithHint True h elTy
               pure (TStream len t)
          Nothing -> bad

    x :-> y ->
      case Cry.tIsFun ty of
        Just (l,r) ->
          do arg <- compileValTypeWithHint False x l
             res <- compileValTypeWithHint False y r
             pure
               case res of
                 TFun as b -> TFun (arg:as) b
                 _         -> TFun [arg] res

        Nothing -> bad

    TupHint hs ->
      case Cry.tIsTuple ty of
        Just ts
          | length ts == length hs ->
            TTuple <$> zipWithM (compileValTypeWithHint nested) hs ts
        _ -> bad


    -- XXX: newtypes also
    RecHint r ->
      case Cry.tIsRec ty of
        Just ts
          | Right mp <- Cry.zipRecords (const (,)) r ts ->
            TTuple <$>
              mapM (uncurry (compileValTypeWithHint nested))
                   (Cry.recordElements mp)
        _ -> bad

  where
  bad :: SpecM a
  bad = panic "matchHint" [ "Hint and type do not match:"
                          , show ("Hint:" <+> pp hint)
                          , show ("Type:" <+> cryPP ty)
                           ]

compileValType :: Bool -> Cry.Type -> SpecM Type
compileValType nested ty =
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
                [ sz ] -> TIntegerMod <$> compileSizeType sz
                _      -> unexpected "Malformed TCIntMod"


            Cry.TCSeq ->
              case ts of
                [tlen,tel] ->
                  do szf <- caseSize tlen
                     case szf of
                       IsInf
                         | nested ->
                           warnUnsupported
                                "Infinite sequences stored in sequences"
                         | otherwise ->
                           TStream IRInfSize <$> compileValType True tel

                       IsFin -> empty
                         -- We do not generate code for sequnece whose
                         -- length would not fit in `usize`

                       IsFinSize ->
                         do sz <- compileSizeType tlen
                            vt <- compileValType True tel
                            case vt of
                              TBool -> pure (TWord sz)
                              TPoly x ->
                                do yes <- caseBool x
                                   pure (if yes then TWord sz
                                                else TArray sz vt)
                              _  -> pure (TArray sz vt)

                _ -> unexpected "Malformed TSeq"

            Cry.TCTuple {}    -> TTuple <$> mapM (compileValType nested) ts

            Cry.TCFun
              | nested -> warnUnsupported "Functions stored in sequences"
              | otherwise ->
              case ts of
                [a,b] ->
                  do let (as,c) = Cry.splitWhile Cry.tIsFun b
                     args <- mapM (compileValType False) (a:as)
                     res  <- compileValType False c
                     pure (TFun args res)
                _ -> unexpected "Malformed function type"

            Cry.TCArray       -> unsupported "Array type"

        Cry.PC {}       -> unexpected "PC"
        Cry.TF {}       -> unexpected "TF"
        Cry.TError {}   -> unexpected "TError"

    Cry.TVar t ->
      case t of
        Cry.TVBound a -> pure (TPoly a)
        Cry.TVFree {} -> unexpected "TVFree"

    Cry.TUser _ _ t     -> compileValType nested t
    Cry.TRec rec -> TTuple <$> mapM (compileValType nested)
                                        (Cry.recordElements rec)
    Cry.TNominal {} -> unsupported "nominal type"    -- XXX

  where
  unexpected msg = panic "compileValType" [msg]

-- | Compile a size type, used the length of a finite sequence.
compileSeqLenSizeType :: Cry.Type -> SpecM Size
compileSeqLenSizeType ty =
  do addNumProps [Cry.tNum maxSizeVal Cry.>== ty]
     compileSizeType ty

compileStreamLenSizeType :: Cry.Type -> SpecM StreamSize
compileStreamLenSizeType ty =
  do inf <- caseIsInf ty
     if inf
        then pure IRInfSize
        else IRSize <$> compileSeqLenSizeType ty


-- | Compile a known finite Cryptol size type to an IR type.
compileSizeType :: Cry.Type -> SpecM Size
compileSizeType ty =
  do sz <- compileStreamSizeType ty
     case sz of
       IRSize s -> pure s
       IRInfSize -> panic "compileSizeType" ["inf"]

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
               IsFinSize -> pure (IRSize (IRPolySize (IRSizeName v MemSize)))
               IsFin     -> pure (IRSize (IRPolySize (IRSizeName v LargeSize)))
        Cry.TVFree {} -> unexpected "Free type variable"

    Cry.TCon tc ts ->
      case tc of
        Cry.TC tcon ->
          case tcon of
            Cry.TCNum n       -> pure (IRSize (IRFixedSize n))
            Cry.TCInf         -> pure IRInfSize

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
          do sz <- caseSize ty
             let finCase rsz =
                    do args <- mapM compileStreamSizeType ts
                       pure (IRSize (evalSizeType tf args rsz))
             case sz of
               IsInf     -> pure IRInfSize
               IsFinSize -> finCase MemSize
               IsFin     -> finCase LargeSize

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNominal {}     -> unexpected "TNominal"
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
          CtrProp (Cry.pFin (op 0))
          `CtrAnd`
          CtrProp (Cry.pFin (op 1))
          `CtrAnd`
          CtrProp (Cry.pFin (op 2))
          `CtrAnd`
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
      Cry.TVar (Cry.TVBound x)      -> CtrIfBool x k CtrTrue
      Cry.TCon (Cry.TC Cry.TCBit) _ -> k
      _                             -> CtrTrue

  ifThenBool t k1 k2 =
    case Cry.tNoUser t of
      Cry.TVar (Cry.TVBound x)      -> CtrIfBool x k1 k2
      Cry.TCon (Cry.TC Cry.TCBit) _ -> k1
      _                             -> k2

  notBool t = ifBool t CtrFalse

  trySolve t nm res =
    case res of
      Cry.Unsolvable  -> CtrFalse
      Cry.SolvedIf xs -> infoFromConstraints xs
      Cry.Unsolved ->
        case Cry.tNoUser t of
          Cry.TVar (Cry.TVBound x) -> CtrTrait (IRTrait nm x)
          Cry.TCon (Cry.TC Cry.TCSeq) [n,a]
            | PRing <- nm, Cry.TVar (Cry.TVBound x) <- Cry.tNoUser a ->
              ifThenBool a (CtrProp (Cry.pFin n))
                           (CtrTrait (IRTrait PRing x))
          _ -> unexpected ("Unsolved not TVar: " ++ show (cryPP t))

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
  | CtrIfBool Cry.TParam ConstraintInfo ConstraintInfo
    -- ^ Currently we do not support arbitrary nested things here,
    -- see `ctrProps`.


-- | Returns traits, unconditional assumptions, and information about
-- if variables are booleans, together with some additional constraints.
-- See `BoolInfo` in "Crypol.Compiler.Cry2IR.SpeclializeM"
ctrProps ::
  ConstraintInfo ->
  Maybe ([Trait], [Cry.Prop], Map Cry.TParam BoolInfo)
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

      CtrIfBool x ifYes ifNo ->
        let yesCase = toProps ifYes []
            noCase  = toTraits ifNo []
        in case Map.findWithDefault (Unknown [] []) x pc of
             Known True ->
               do newPs <- yesCase
                  pure (ts, newPs ++ pu, pc)
             Known False ->
               do newTs <- noCase
                  pure (newTs ++ ts, pu, pc)
             Unknown y n ->
                case (yesCase, noCase) of
                  (Nothing,Nothing) -> Nothing
                  (Nothing,Just newTs) ->
                     pure (newTs ++ n ++ ts, pu, Map.insert x (Known False) pc)
                  (Just newPs,Nothing) ->
                     pure (ts, newPs ++ y ++ pu, Map.insert x (Known True) pc)
                  (Just newPs, Just newTs) ->
                    pure (ts, pu, Map.insert x (Unknown (newPs ++ y)
                                                        (newTs ++ n)) pc)


  toTraits :: ConstraintInfo -> [Trait] -> Maybe [Trait]
  toTraits ci ts =
    case ci of
      CtrFalse      -> Nothing
      CtrTrue       -> pure ts
      CtrAnd p1 p2  -> toTraits p2 =<< toTraits p1 ts
      CtrTrait t    -> pure (t:ts)
      CtrProp {}    -> panic "toTraits" ["CtrProp"]
      CtrIfBool {}  -> panic "toTriats" ["CtrIfBool"]

  toProps :: ConstraintInfo -> [Cry.Prop] -> Maybe [Cry.Prop]
  toProps ci ts =
    case ci of
      CtrFalse      -> Nothing
      CtrTrue       -> pure ts
      CtrAnd p1 p2  -> toProps p2 =<< toProps p1 ts
      CtrProp t     -> pure (t:ts)
      CtrTrait {}   -> panic "toProps" ["CtrTrait"]
      CtrIfBool {}  -> panic "toProps" ["CtrIfBool"]


--------------------------------------------------------------------------------

---- | Assumptions we can make on the number type parameters.
data PropInfo = PropInfo
  { propVars  :: [Cry.TParam]
  , propAsmps :: [Cry.Prop]
  }

instance PP PropInfo where
  pp info =
    hang ("forall" <+> commaSep (map cryPP (propVars info)) <.> ".") 2
         (vcat (map cryPP (propAsmps info)))

