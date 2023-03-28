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

testSpec :: Cry.Schema -> M.CryC [(([Type],Type), Map Cry.TParam TConstraint)]
testSpec sch =
  runSpecM
    do SpecM $ set RW { roTParams = Cry.sVars sch
                      , rwProps = mapMaybe toNumP (Cry.sProps sch)
                      , rwConstraints = mempty
                      }
       res <- compileFunType (Cry.sType sch)
       pure res


  where
  toNumP p =
    case p of
      Cry.TUser _ _ t -> toNumP t
      Cry.TCon (Cry.PC Cry.PLiteral) [val,_] -> Just (Cry.pFin val)
      _ | Cry.isNumeric p -> Just p
        | otherwise -> Nothing

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

checkPossible :: SpecM ()
checkPossible =
  do solver  <- doCryC M.getSolver
     rw      <- SpecM get
     let tparams = roTParams rw
         props1  = rwProps rw
         props2  = [ sizeProp s (Cry.TVar (Cry.tpVar x))
                   | (x,SizeConstraint s) <- Map.toList (rwConstraints rw)
                   ]
     bad <- doIO $ Cry.inNewFrame solver
       do tvars <- Cry.declareVars solver (map Cry.tpVar tparams)
          Cry.unsolvable solver tvars (props2 ++ props1)
     when bad empty

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
addProp p = SpecM $ sets_ \s -> s { rwProps = p : rwProps s }

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

runSpecM :: SpecM a -> M.CryC [(a, Map Cry.TParam TConstraint)]
runSpecM (SpecM m) =
  do res <- findAll (runStateT rw0 m)
     pure [ (a, rwConstraints b) | (a,b) <- res ]
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
            Cry.TCNum {}         -> unexpected "TCNum"
            Cry.TCInf            -> unexpected "TCInf"

            Cry.TCBit            -> pure TBool
            Cry.TCInteger        -> pure TInteger
            Cry.TCRational       -> pure TRational

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
    Cry.TUser _ _ t     -> compileStreamSizeType t

    Cry.TVar t          -> case t of
                             Cry.TVBound v -> pure (IRSize (IRPolySize v))
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
          do args <- mapM compileStreamSizeType ts
             let dflt = IRSize (IRComputedSize tf args)
             pure case tf of
                    Cry.TCAdd           -> liftInfNat dflt args Cry.nAdd
                    Cry.TCSub           -> liftInfNat dflt args Cry.nSub
                    Cry.TCMul           -> liftInfNat dflt args Cry.nMul
                    Cry.TCDiv           -> liftInfNat dflt args Cry.nDiv
                    Cry.TCMod           -> liftInfNat dflt args Cry.nMod
                    Cry.TCExp           -> liftInfNat dflt args Cry.nExp
                    Cry.TCWidth         -> liftInfNat dflt args Cry.nWidth
                    Cry.TCMin           -> liftInfNat dflt args Cry.nMin
                    Cry.TCMax           -> liftInfNat dflt args Cry.nMax
                    Cry.TCCeilDiv       -> liftInfNat dflt args Cry.nCeilDiv
                    Cry.TCCeilMod       -> liftInfNat dflt args Cry.nCeilMod
                    Cry.TCLenFromThenTo ->
                       liftInfNat dflt args Cry.nLenFromThenTo

        Cry.PC {}       -> unexpected "PC"
        Cry.TError {}   -> unexpected "TError"

    Cry.TRec {}         -> unexpected "TRec"
    Cry.TNewtype {}     -> unexpected "TNewtype"
  where
  unexpected x = panic "compileStreamSizeType" [x]


class LiftInfNat a where
  liftInfNat :: StreamSize -> [StreamSize] -> a -> StreamSize

instance LiftInfNat a => LiftInfNat (Maybe a) where
  liftInfNat dflt xs a =
    case a of
      Just v  -> liftInfNat dflt xs v
      Nothing -> panic "liftInfNat" ["Malformed size type: partial"]

instance LiftInfNat Cry.Nat' where
  liftInfNat _ xs a =
    case xs of
      [] ->
        case a of
          Cry.Inf   -> IRInfSize
          Cry.Nat n -> IRSize (IRFixedSize n)
      _  -> panic "liftInfNat" ["Malformed size type: extra arguments"]

instance LiftInfNat a => LiftInfNat (Cry.Nat' -> a) where
  liftInfNat dflt xs f =
    case xs of
      y : ys ->
        case y of
          IRInfSize              -> liftInfNat dflt ys (f Cry.Inf)
          IRSize (IRFixedSize i) -> liftInfNat dflt ys (f (Cry.Nat i))
          _                      -> dflt

      [] -> panic "liftInfNat" ["Malformed size type: missing arguments"]
