module Cryptol.Compiler.Cry2IR.Monad
  ( SpecM
  , runSpecM

    -- * Lifting
  , doIO
  , doCryC

    -- * Errors
  , unsupported
  , M.panic

    -- * Solver
  , checkPossible

    -- * Constraints
  , BoolInfo(..)
  , SizeConstraint(..)
  , addTParams
  , addTrait
  , addIsBoolProp
  , addNumProps
  , caseSize
  , caseIsInf
  , caseBool

    -- * Queries
  , getTParams
  , getTraits
  , getBoolConstraint
  , getSubst

  ) where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import Control.Applicative(Alternative(..),asum)
import MonadLib

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.SMT qualified as Cry

import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.Subst




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
  , roTraits      :: Map Cry.TParam [Trait]  -- indexed by variable
  , rwProps       :: [Cry.Prop]
  , rwBoolProps   :: Map Cry.TParam BoolInfo
  }

data BoolInfo =
    Known   Bool       -- ^ The parameter is/isn't bool on this path.
  | Unknown [Cry.Prop] -- ^ If parameter is bool, then also add props.

instance BaseM SpecM SpecM where
  inBase = id

-- | Run a computation with no constraints or parameters
runSpecM :: SpecM a -> M.CryC [a]
runSpecM (SpecM m) = map fst <$> findAll (runStateT rw0 m)
  where
  rw0 = RW { roTParams     = mempty
           , roTraits      = mempty
           , rwProps       = mempty
           , rwBoolProps   = mempty
           }

-- | Do some IO.
doIO :: IO a -> SpecM a
doIO m = doCryC (M.doIO m)

-- | Do CryC stuff.
doCryC :: M.CryC a -> SpecM a
doCryC m = SpecM (inBase m)

-- | Abort: we found something that's unsupported.
unsupported :: Text -> SpecM a
unsupported x = doCryC (M.unsupported x)



--------------------------------------------------------------------------------
-- Boolean constraint

-- | Get known constraints on a specific vairable.
getBoolConstraint :: Cry.TParam -> SpecM BoolInfo
getBoolConstraint x =
  SpecM (Map.findWithDefault (Unknown []) x . rwBoolProps <$> get)

-- | Add a constraint on a (value) type parameter.
addIsBoolProp :: Cry.TParam -> BoolInfo -> SpecM ()
addIsBoolProp x t =
  do info <- getBoolConstraint x
     case info of
       Known yes ->
         case t of
           Known yes' -> when (yes /= yes') empty
           Unknown ps
             | yes       -> addNumProps ps
             | otherwise -> pure ()
       Unknown ps ->
         case t of
           Known yes  -> when yes (addNumProps ps) >> setI t
           Unknown qs ->
             do let new = qs ++ ps
                ok <- couldAddNumProps new
                if ok then setI (Unknown (qs ++ ps))
                      else setI (Known False)
  where
  setI i =
    do case i of
         Known True ->
           SpecM $ sets_ \s -> s { roTraits = Map.delete x (roTraits s) }
         _ -> pure ()
       SpecM $ sets_ \s -> s { rwBoolProps = Map.insert x i (rwBoolProps s) }


--------------------------------------------------------------------------------
-- Numeric constraints

couldAddNumProps :: [Cry.Prop] -> SpecM Bool
couldAddNumProps ps =
  case ps of
    [] -> pure True
    _  -> case asum (map Cry.tIsError ps) of
            Just _ -> pure False
            _  ->
              do props <- SpecM (rwProps <$> get)
                 SpecM $ sets_ \s -> s { rwProps = ps ++ props }
                 imp <- checkImpossibleBool
                 SpecM $ sets_ \s -> s { rwProps = props }
                 pure (not imp)



-- | Add some numeric properties to the current thread.
addNumProps :: [Cry.Prop] -> SpecM ()
addNumProps ps =
  case ps of
    [] -> pure ()
    _  -> case asum (map Cry.tIsError ps) of
            Just _ -> empty
            _  ->
              do SpecM $ sets_ \s -> s { rwProps = ps ++ rwProps s }
                 checkPossible


data SizeConstraint =
    IsFin
  | IsFinSize
  | IsInf
    deriving Eq

-- | Convert a size constraint to a property on a type.
sizeProp :: SizeConstraint -> Cry.Type -> [Cry.Prop]
sizeProp s t =
  case s of
    IsFinSize -> [Cry.pFin t, Cry.tNum (lim - 1) Cry.>== t ]
    IsFin     -> [Cry.pFin t, t Cry.>== Cry.tNum lim ]
    IsInf     -> [t Cry.=#= Cry.tInf]
  where
  lim = 2^(64::Int) - 1 :: Integer


--------------------------------------------------------------------------------
-- Interactions with the solver

prepSolver :: (Cry.Solver -> [Cry.TParam] -> [Cry.Prop] -> IO a) -> SpecM a
prepSolver k =
  do solver  <- doCryC M.getSolver
     rw      <- SpecM get
     doIO (k solver (roTParams rw) (rwProps rw))


-- | Check if the current set of properties are consistent.
checkImpossibleBool :: SpecM Bool
checkImpossibleBool =
  prepSolver \solver tparams props ->
     Cry.inNewFrame solver
        do tvars <- Cry.declareVars solver (map Cry.tpVar tparams)
           Cry.unsolvable solver tvars props

-- | Check if the current set of properties are consistent.
-- Kill the thread if not.
checkPossible :: SpecM ()
checkPossible =
  do imp <- checkImpossibleBool
     when imp empty


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

-- | This should be done before adding constraints about the parameters.
addTParams :: [Cry.TParam] -> SpecM ()
addTParams as = SpecM $ sets_ \s -> s { roTParams = as ++ roTParams s }

getTParams :: SpecM [Cry.TParam]
getTParams = SpecM (roTParams <$> get)

addTrait :: Trait -> SpecM ()
addTrait t@(IRTrait _ x) =
  SpecM $ sets_ \s -> s { roTraits = Map.insertWith (++) x [t] (roTraits s) }

getTraits :: SpecM [Trait]
getTraits = SpecM (concat . Map.elems . roTraits <$> get)



--------------------------------------------------------------------------------

-- | Returns if this type parameter is Bool.
-- May split the world.
caseBool :: Cry.TParam -> SpecM Bool
caseBool x = tryCase True <|> tryCase False
  where
  tryCase t = addIsBoolProp x (Known t) >> pure t

-- | Returns the class for this type.
-- May split the world.
caseSize :: Cry.Type -> SpecM SizeConstraint
caseSize ty = tryCase IsInf <|> tryCase IsFin <|> tryCase IsFinSize
  where
  tryCase p = addNumProps (sizeProp p ty) >> pure p

-- | Returns `True` if the given type is `inf`.
-- May split the world.
caseIsInf :: Cry.Type -> SpecM Bool
caseIsInf ty = tryCase True <|> tryCase False
  where
  tryCase isInf =
    do addNumProps [ if isInf then ty Cry.=#= Cry.tInf else Cry.pFin ty ]
       pure isInf





--------------------------------------------------------------------------------

-- | Find type parameters that can have only a single value.
-- Note that this is not cached, so it will do the work each time it is called.
getSubst :: SpecM Subst
getSubst =
  do fi <- checkFixedSize
     let su1 = Map.foldrWithKey suAddSize suEmpty fi
     cs <- rwBoolProps <$> SpecM get
     pure (Map.foldrWithKey addT su1 cs)
  where
  addT x info su =
    case info of
      Known True -> suAddType x TBool su
      _          -> su

checkFixedSize :: SpecM (Map Cry.TParam StreamSize)
checkFixedSize =
  do vs <- roTParams <$> SpecM get
     let nums = filter ((== Cry.KNum) . Cry.kindOf) vs
     foldM checkVar mempty nums
  where
  checkVar done x =
    do mb1 <- checkSingleValue x
       case mb1 of
         Nothing -> pure done
         Just v  -> pure (Map.insert x t done)
           where t = case v of
                       Cry.Inf   -> IRInfSize
                       Cry.Nat n -> IRSize (IRFixedSize n)


