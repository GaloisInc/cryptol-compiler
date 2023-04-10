module Cryptol.Compiler.Cry2IR.Specialize
  (testSpec
  ) where

import Data.Map(Map)
import Data.Map qualified as Map
import Control.Applicative(Alternative(..))
import Control.Monad(forM_)

import Cryptol.TypeCheck.TCon qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.IR
import Cryptol.Compiler.IR.Subst

import Cryptol.Compiler.Cry2IR.Monad

testSpec :: Cry.Schema -> M.CryC [ (Subst,[Type],Type) ]
testSpec sch =
  case ctrProps (infoFromConstraints (Cry.sProps sch)) of
    -- inconsistent
    Nothing -> pure []
    Just (props,boolProps) ->
      runSpecM
        do addTParams (Cry.sVars sch)
           addNumProps props
           forM_ (Map.toList boolProps) \(x,ps) ->
             case ps of
               [] -> addIsBoolProp x (Known False)
               ps -> addIsBoolProp x (Unknown ps)

           (args,res) <- compileFunType (Cry.sType sch)
           su  <- getSubst
           pure (su, apSubst su args, apSubst su res)


{-
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
-}



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



