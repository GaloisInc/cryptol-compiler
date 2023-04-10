module Cryptol.Compiler.Specialize (testSpec, TConstraint(..), ppSpecMap) where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import Control.Applicative(Alternative(..),asum)
import MonadLib

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.SMT qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.Monad (panic)
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.Subst


testSpec :: Cry.Schema -> M.CryC [ (Subst,[Type],Type) ]
testSpec sch =
  case ctrProps (infoFromConstraints (Cry.sProps sch)) of
    -- inconsistent
    Nothing -> pure []
    Just (props,null_conds) ->
      let (notBool,conds) = Map.partition null null_conds
      in
      runSpecM
        do SpecM $ set RW { roTParams     = Cry.sVars sch
                          , rwProps       = props
                          , rwBoolProps   = conds
                          , rwConstraints = IsNotBool <$ notBool
                          }
           (args,res) <- compileFunType (Cry.sType sch)
           su  <- getSubst
           pure (su, apSubst su args, apSubst su res)


getSubst :: SpecM Subst
getSubst =
  do fi <- checkFixedSize
     let su1 = Map.foldrWithKey suAddSize suEmpty fi
     cs <- rwConstraints <$> SpecM get
     pure (Map.foldrWithKey addT su1 cs)
  where
  addT x c su =
    case c of
      IsBool -> suAddType x TBool su
      _      -> su



--------------------------------------------------------------------------------
newtype SpecM a = SpecM (SpecImpl a)
  deriving (Functor,Applicative,Monad,Alternative) via SpecImpl

-- | This is the implementation of the monad
type SpecImpl =
  WithBase M.CryC
    '[ StateT RW
     , ChoiceT
     ]

data RW = RW
  { roTParams     :: [Cry.TParam]
  , rwProps       :: [Cry.Prop]
  , rwBoolProps   :: Map Cry.TParam [Cry.Prop] -- on paths when this is bool
  , rwConstraints :: Map Cry.TParam TConstraint
  }

instance BaseM SpecM SpecM where
  inBase = id

doIO :: IO a -> SpecM a
doIO m = doCryC (M.doIO m)

doCryC :: M.CryC a -> SpecM a
doCryC m = SpecM (inBase m)

data TConstraint =
    IsBool
  | IsNotBool
  | SizeConstraint SizeConstraint
    deriving Eq

data SizeConstraint =
    IsFin
  | IsFinSize
  | IsInf
    deriving Eq



--------------------------------------------------------------------------------
prepSolver :: (Cry.Solver -> [Cry.TParam] -> [Cry.Prop] -> IO a) -> SpecM a
prepSolver k =
  do solver  <- doCryC M.getSolver
     rw      <- SpecM get
     let tparams = roTParams rw
         props1  = rwProps rw
         props2  = [ p
                   | (x,SizeConstraint s) <- Map.toList (rwConstraints rw)
                   , p <- sizeProp s (Cry.TVar (Cry.tpVar x))
                   ]
     doIO (k solver tparams (props2 ++ props1))


-- | Check if the current set of properties are consistent.
checkPossible :: SpecM ()
checkPossible =
  do bad <- prepSolver \solver tparams props ->
              Cry.inNewFrame solver
                 do tvars <- Cry.declareVars solver (map Cry.tpVar tparams)
                    Cry.unsolvable solver tvars props
     when bad empty


-- | Check if the given variable can have only one possible value.
checkSingleValue :: Cry.TParam -> SpecM (Maybe Cry.Nat')
checkSingleValue x' =
  prepSolver \solver tparams props ->
    do let as = map Cry.tpVar tparams
           x  = Cry.tpVar x'
       model1 <- Cry.tryGetModel solver as props
       case model1 of
         Just mo | Just v <- lookup x mo ->
           do let notThis = Cry.TVar x Cry.=/= Cry.tNat' v
              model2 <- Cry.tryGetModel solver as (notThis : props)
              case model2 of
                Nothing -> pure (Just v)
                _       -> pure Nothing
         _ -> pure Nothing

--------------------------------------------------------------------------------


sizeProp :: SizeConstraint -> Cry.Type -> [Cry.Prop]
sizeProp s t =
  case s of
    IsFinSize -> [Cry.pFin t, Cry.tNum (lim - 1) Cry.>== t ]
    IsFin     -> [Cry.pFin t, t Cry.>== Cry.tNum lim ]
    IsInf     -> [t Cry.=#= Cry.tInf]
  where
  lim = 2^(64::Int) - 1 :: Integer

getConstraint :: Cry.TParam -> SpecM (Maybe TConstraint)
getConstraint x = SpecM (Map.lookup x . rwConstraints <$> get)

isBool :: Cry.TParam -> SpecM (Maybe Bool)
isBool x = fmap (IsBool ==) <$> getConstraint x

setConstraint :: Cry.TParam -> TConstraint -> SpecM ()
setConstraint x t =
  do check <-
       SpecM $
         sets \rw ->
           let rw1 = rw { rwConstraints = Map.insert x t (rwConstraints rw) }
           in case t of
                IsBool | Just ps <- Map.lookup x (rwBoolProps rw1) ->
                  (True, rw1 { rwProps = ps ++ rwProps rw1 })
                _ -> (False, rw1)
     when check checkPossible

caseBool :: Cry.TParam -> SpecM Bool
caseBool x =
  do mb <- isBool x
     case mb of
       Just yes -> pure yes
       Nothing ->
         (setConstraint x IsBool >> pure True)
         <|>
         (setConstraint x IsNotBool >> pure False)

addProp :: [Cry.Prop] -> SpecM ()
addProp p =
  case asum (map Cry.tIsError p) of
    Just _ -> empty
    _      -> SpecM $ sets_ \s -> s { rwProps = p ++ rwProps s }

caseSize :: Cry.Type -> SpecM SizeConstraint
caseSize ty = tryCase IsInf <|> tryCase IsFin <|> tryCase IsFinSize
  where
  tryCase p =
    do case Cry.tNoUser ty of
         Cry.TVar (Cry.TVBound x) ->
           do mb <- getConstraint x
              case mb of
                Nothing -> setConstraint x (SizeConstraint p)
                Just ct -> unless (SizeConstraint p == ct) empty
         _ -> addProp (sizeProp p ty)
       checkPossible
       pure p

caseIsInf :: Cry.Type -> SpecM Bool
caseIsInf ty = tryCase True <|> tryCase False
  where
  tryCase isInf =
    do addProp [ if isInf then ty Cry.=#= Cry.tInf else Cry.pFin ty ]
       checkPossible
       pure isInf




unsupported :: Text -> SpecM a
unsupported x = doCryC (M.unsupported x)

runSpecM :: SpecM a -> M.CryC [a]
runSpecM (SpecM m) = map fst <$> findAll (runStateT rw0 m)
  where
  rw0 = RW { roTParams = mempty
           , rwProps = mempty, rwBoolProps = mempty
           , rwConstraints = mempty
           }
--------------------------------------------------------------------------------


ppSpecMap :: Map Cry.TParam TConstraint -> Doc
ppSpecMap m = vcat [ ppC x c | (x,c) <- Map.toList m ]
  where
  ppC x c =
    case c of
      IsBool    -> pp x <+> "==" <+> "Bool"
      IsNotBool -> pp x <+> "!=" <+> "Bool"
      SizeConstraint sc -> ppSizeC x sc

  ppSizeC x c =
    case c of
      IsFin     -> "fin" <+> pp x
      IsFinSize -> "fin_size" <+> pp x
      IsInf     -> pp x <+> "==" <+> "inf"


--------------------------------------------------------------------------------
checkFixedSize :: SpecM (Map Cry.TParam StreamSize)
checkFixedSize =
  do vs <- roTParams <$> SpecM get
     let nums = filter ((== Cry.KNum) . Cry.kindOf) vs
     foldM checkVar mempty nums
  where
  checkVar done x =
    do mb <- getConstraint x
       case mb of
         Just (SizeConstraint IsInf) -> pure (Map.insert x IRInfSize done)
         _ -> do mb1 <- checkSingleValue x
                 case mb1 of
                   Nothing -> pure done
                   Just v  -> pure (Map.insert x t done)
                     where t = case v of
                                 Cry.Inf   -> IRInfSize
                                 Cry.Nat n -> IRSize (IRFixedSize n)


--------------------------------------------------------------------------------
compileFunType :: Cry.Type -> SpecM ([Type],Type)
compileFunType = go []
  where
  go args ty =
    case ty of
      Cry.TUser _ _ ty1 -> go args ty1
      Cry.TCon (Cry.TC Cry.TCFun) [a,b] ->
        do a' <- compileValType a
           go (a' : args) b
      _ ->
        do r <- compileValType ty
           pure (reverse args,r)


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
                [ e, p ] ->
                  do ce <- compileStreamSizeType e
                     cp <- compileStreamSizeType p
                     case (ce,cp) of
                       (IRSize (IRFixedSize pr), IRSize (IRFixedSize ex))
                          | pr == 8  && ex == 24 -> pure TFloat
                          | pr == 11 && ex == 53 -> pure TDouble
                       _ -> unsupported "floating point type"
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
                       IsFin -> empty
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
                                       pure (if yes then TWord sz else TArray sz vt)
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
infoFromConstraints :: [Cry.Prop] -> ConstraintInfo
infoFromConstraints = foldr (CtrAnd . infoFromConstraint) CtrTrue

infoFromConstraint :: Cry.Prop -> ConstraintInfo
infoFromConstraint prop =
  case Cry.tNoUser prop of
    Cry.TCon (Cry.PC c) ts ->
      case c of
        Cry.PEqual           -> CtrProp prop
        Cry.PNeq             -> CtrProp prop
        Cry.PGeq             -> CtrProp prop
        Cry.PFin             -> CtrProp prop
        Cry.PPrime           -> CtrProp prop

        Cry.PHas {}          -> CtrTrue

        Cry.PZero            -> CtrTrue
        Cry.PLogic           -> CtrTrue
        Cry.PRing            -> notBool (op 0)
        Cry.PIntegral        -> notBool (op 0)
        Cry.PField           -> notBool (op 0)
        Cry.PRound           -> notBool (op 0)

        Cry.PEq              -> CtrTrue
        Cry.PCmp             -> CtrTrue
        Cry.PSignedCmp       -> notBool (op 0)

        Cry.PLiteral ->
          let n = op 0
          in CtrAnd
               (CtrProp (Cry.pFin n))
               (ifBool (op 1) (CtrProp (Cry.tNum (1::Integer) Cry.>== n)))

        Cry.PLiteralLessThan ->
          ifBool (op 1) (CtrProp (Cry.tNum (2::Integer) Cry.>== op 0))

        Cry.PFLiteral        -> notBool (op 2)

        Cry.PValidFloat {}   -> CtrTrue -- XXX: restrict to supported ones?
        Cry.PAnd             -> infoFromConstraints ts
        Cry.PTrue            -> CtrTrue
      where
      op n =
        case splitAt n ts of
          (_,t:_) -> t
          _       -> panic "infoFromConstraint" ["Expected 1 argument"]


    _ -> CtrTrue

  where
  ifBool t k =
    case Cry.tNoUser t of
      Cry.TVar (Cry.TVBound x)      -> CtrIfBool x k
      Cry.TCon (Cry.TC Cry.TCBit) _ -> k
      _                             -> CtrTrue

  notBool t = ifBool t CtrFalse

--------------------------------------------------------------------------------

data ConstraintInfo =
    CtrFalse
  | CtrTrue
  | CtrAnd ConstraintInfo ConstraintInfo
  | CtrProp Cry.Prop
  | CtrIfBool Cry.TParam ConstraintInfo
    -- ^ Currently we do not support arbitrary nested things here,
    -- see `ctrProps`.

-- | Returns unconditional assumptions, and ones that depend on the given
-- parameter being bool.  If the entry is `[]`, than the parameter must
-- not be bool.  If it we have some props, then they can be assumed but
-- only when the parameter *is* bool.
ctrProps ::
  ConstraintInfo -> Maybe ([Cry.Prop], Map Cry.TParam [Cry.Prop])
ctrProps = go (mempty,mempty)
  where
  go props@(pu,pc) ci =
    case ci of
      CtrFalse   -> Nothing
      CtrTrue    -> pure props
      CtrAnd x y ->
        do p1 <- go props x
           go p1 y

      CtrProp p -> pure (p : pu, pc)

      CtrIfBool x pr ->
        pure
          case pr of
            CtrFalse  -> (pu, Map.insert x [] pc)
            CtrProp p ->
              case Map.lookup x pc of
                Just [] -> props
                _       -> (pu, Map.insertWith (++) x [p] pc)
            _ -> panic "ctrProps" ["Unexpected nested prop"]



