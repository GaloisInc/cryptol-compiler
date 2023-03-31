module Cryptol.Compiler.Specialize (testSpec, TConstraint(..), ppSpecMap) where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Maybe(mapMaybe)
import Control.Applicative(Alternative(..))
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
  runSpecM
    do SpecM $ set RW { roTParams = Cry.sVars sch
                      , rwProps = mapMaybe toNumP (Cry.sProps sch)
                      , rwConstraints = mempty
                      }
       (args,res) <- compileFunType (Cry.sType sch)
       su  <- getSubst
       pure (su, apSubst su args, apSubst su res)

  where
  toNumP p =
    case p of
      Cry.TUser _ _ t -> toNumP t
      Cry.TCon (Cry.PC Cry.PLiteral) [val,_] -> Just (Cry.pFin val)
      _ | Cry.isNumeric p -> Just p
        | otherwise -> Nothing

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
  | IsInf
    deriving Eq



--------------------------------------------------------------------------------
prepSolver :: (Cry.Solver -> [Cry.TParam] -> [Cry.Prop] -> IO a) -> SpecM a
prepSolver k =
  do solver  <- doCryC M.getSolver
     rw      <- SpecM get
     let tparams = roTParams rw
         props1  = rwProps rw
         props2  = [ sizeProp s (Cry.TVar (Cry.tpVar x))
                   | (x,SizeConstraint s) <- Map.toList (rwConstraints rw)
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


sizeProp :: SizeConstraint -> Cry.Type -> Cry.Prop
sizeProp s t =
  case s of
    IsFin -> Cry.pFin t
    IsInf -> t Cry.=#= Cry.tInf

getConstraint :: Cry.TParam -> SpecM (Maybe TConstraint)
getConstraint x = SpecM (Map.lookup x . rwConstraints <$> get)

isBool :: Cry.TParam -> SpecM (Maybe Bool)
isBool x = fmap (IsBool ==) <$> getConstraint x

setConstraint :: Cry.TParam -> TConstraint -> SpecM ()
setConstraint x t =
  SpecM (sets_ \rw -> rw { rwConstraints = Map.insert x t (rwConstraints rw) })

caseBool :: Cry.TParam -> SpecM Bool
caseBool x =
  do mb <- isBool x
     case mb of
       Just yes -> pure yes
       Nothing ->
         (setConstraint x IsBool >> pure True)
         <|>
         (setConstraint x IsNotBool >> pure False)

addProp :: Cry.Prop -> SpecM ()
addProp p =
  case Cry.tIsError p of
    Just _ -> empty
    _      -> SpecM $ sets_ \s -> s { rwProps = p : rwProps s }

caseSize :: Cry.Type -> SpecM SizeConstraint
caseSize ty = tryCase IsInf <|> tryCase IsFin
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


unsupported :: Text -> SpecM a
unsupported x = doCryC (M.unsupported x)

runSpecM :: SpecM a -> M.CryC [a]
runSpecM (SpecM m) = map fst <$> findAll (runStateT rw0 m)
  where
  rw0 = RW { roTParams = mempty, rwProps = mempty, rwConstraints = mempty }
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
      IsFin -> "fin" <+> pp x
      IsInf -> pp x <+> "==" <+> "inf"


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
           pure (args,r)


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
                     isize <- compileStreamSizeType tlen
                     vt    <- compileValType tel
                     case szf of
                       IsInf -> pure (TStream IRInfSize vt)
                       IsFin ->
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
        Cry.TVBound v -> checkInf (pure (IRSize (IRPolySize v)))
        Cry.TVFree {} -> unexpected "TVFree"

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
          checkInf
            do args <- mapM compileStreamSizeType ts
               pure (evalSizeType tf args)

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNewtype {}     -> unexpected "TNewtype"
  where
  unexpected x = panic "compileStreamSizeType" [x]
  checkInf k =
    do ctr <- caseSize ty
       case ctr of
         IsInf -> pure IRInfSize
         IsFin -> k


