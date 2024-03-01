module Cryptol.Compiler.Cry2IR.ChooseSpec (selectInstance) where

import Data.Set(Set)
import Data.Set qualified as Set
import Data.Either(partitionEithers)
import MonadLib

import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.Monad qualified as M

import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.IR.Free
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.IR.EvalType
import Cryptol.Compiler.Cry2IR.ConvertM
import Cryptol.Compiler.Cry2IR.InstanceMap
import Cryptol.Compiler.Cry2IR.Type
import Cryptol.Compiler.Cry2IR.RepHints


type M = ExceptionT () ConvertM


{- | Select an instantiation of a function to use.
Note that the selected instantitation might not match the desired
type exectly, so we might still have to do a cast after the call. -}
selectInstance ::
  Cry.Name      {- ^ Call this function -} ->
  [Cry.Type]    {- ^ With these type arguments -} ->
  Type          {- ^ And this is what we want to get -} ->
  ConvertM Call
selectInstance f tyArgs tgtT =
  do -- _ <- doIO (putStrLn "XXX")
     instDB <- doCryC (M.getFun f)
     let is = instanceMapToList instDB
     targs <- mapM prepTArg tyArgs
     select is Nothing targs is
  where
  select is0 candidate targs is =
    case is of
      [] ->
        case candidate of
          Nothing ->
            unsupported $ vcat
               [ "Unsupported function instance:"
               , "Function:" <+> pp f
               , "Type arguments:" <+> commaSep (map (either pp cryPP) targs)
               , "Target type:" <+> pp tgtT
               ]
          Just (c,_)  -> pure c
      i : more ->
        do mbYes <- matchFun i targs tgtT
           case mbYes of
             Nothing -> select is0 candidate targs more
             Just this@(c,hint) ->
               case hint of
                 NoHint -> pure c
                 _ ->
                    let newCandidate =
                          case candidate of
                            Nothing     -> this
                            Just other  -> pickBetter this other
                    in select is0 (Just newCandidate) targs more

  prepTArg t =
    case Cry.kindOf t of
      Cry.KNum -> Left <$> compileStreamSizeType t
      _        -> pure (Right t)

  pickBetter (h1,c1) (h2,c2) =
    if betterHint h1 h2 then (h1,c1) else (h2,c2)

  -- XXX: ????
  betterHint _ _ = False  -- we alwyas pick the first one.



liftMaybe :: Maybe a -> M a
liftMaybe = maybe (raise ()) pure

runMatch :: M a -> ConvertM (Maybe a)
runMatch m =
  do res <- runExceptionT m
     pure
        case res of
          Left {} -> Nothing
          Right a -> Just a

-- | Check if some concrete type arguments match a particular
-- function instance.  Returns (type arguments, size arguments)
matchFun ::
  (FunName, FunType)           {- ^ Instance under consideration -} ->
  [Either StreamSize Cry.Type] {- ^ Type parameters, numeric ones compiled-} ->
  Type                         {- ^ Target type -} ->
  ConvertM (Maybe (Call, RepHint))
  -- ^ Nothing if the instnace does not apply.
  -- Just (call,hint) if we can use the instance, the hint gives transformation
  -- we have to apply to the result if we use this instance.

matchFun (f, funTy) tyArgs tgtT =
  runMatch
  do let FunInstance pinfo = irfnInstance f
     matchPs <- liftMaybe (zipWithM matchParamInfo pinfo tyArgs)
     let (nums, vals) = partitionEithers (concat matchPs)
     let numPs = ftSizeParams funTy
         valPs = ftTypeParams funTy

     unless (length nums == length numPs) $
        panic "matchFun" ["Numeric argument mismatch"]
     unless (length vals == length valPs) $
        panic "matchFun" ["Value type argument mismatch"]

     let numSu = foldr (\(x,sz) -> suAddSize (irsName x) (IRSize sz)) suEmpty
                       (zip numPs nums)

     let nestedPs = nestedTyParams (TFun (ftParams funTy) (ftResult funTy))
         translateIf p su (x,t)
           | p x =
             do t' <- compileValType t
                pure (suAddType x t' su)
           | otherwise = pure su

     nestedSu <-
       lift (foldM (translateIf (`Set.member` nestedPs)) numSu (zip valPs vals))

     let resT = apSubst nestedSu (ftResult funTy)

     (hint,moreSu) <- liftMaybe (matchType resT tgtT)

     let resSu = suMerge moreSu nestedSu

     finalSu <-
        lift (foldM (translateIf (not . (`suBinds` resSu))) resSu
             (zip valPs vals))

     let fromMb x mb =
           case mb of
             Just a -> a
             Nothing -> panic "numInst" ["Missing type parameter:",show (pp x)]

     let numInst = [ ( streamSizeToSize (fromMb x (suLookupSize x finalSu))
                     , irsSize n
                     )
                   | n <- numPs, let x = irsName n ]
         valInst = [ fromMb x (suLookupType x finalSu) | x <- valPs ]
         topCall = IRTopFunCall
                     { irtfName = f
                     , irtfTypeArgs = valInst
                     , irtfSizeArgs = numInst
                     }
         call = IRCall
                  { ircFun      = IRTopFun topCall
                  , ircFunType  = funTy
                  , ircArgTypes = apSubst finalSu (ftParams funTy)
                  , ircResType  = apSubst finalSu (ftResult funTy)
                  , ircArgs     = []
                  }
     pure (call, hint)

-- | Type parameters that appear as elements in other sequences.
nestedTyParams :: Type -> Set Cry.TParam
nestedTyParams ty =
  case ty of
    TTuple ts       -> Set.unions (map nestedTyParams ts)
    TFun as b       -> Set.unions (map nestedTyParams (b:as))
    TArray _ elTy   -> freeValTypeVars elTy
    TStream _ elTy  -> freeValTypeVars elTy

    TWord {}        -> mempty
    TInteger        -> mempty
    TIntegerMod {}  -> mempty
    TRational       -> mempty
    TFloat          -> mempty
    TDouble         -> mempty
    TBool           -> mempty
    TPoly {}        -> mempty



-- | Numeric on the left
matchParamInfo ::
  ParamInfo ->
  Either StreamSize Cry.Type ->
  Maybe [Either Size Cry.Type]
matchParamInfo pinfo t =
  case pinfo of

    NumFixed Cry.Inf ->
      case t of
        Left IRInfSize -> pure []
        _ -> Nothing

    NumFixed (Cry.Nat n) ->
      case t of
        Left (IRSize (IRFixedSize m)) | m == n -> pure []
        _ -> Nothing

    NumVar need ->
      case t of
        Left (IRSize s) ->
          let ok = pure [Left s]
              haveSize = sizeTypeSize s
          in if need == haveSize then ok else Nothing
          -- XXX: Is there a problem here, because `sizeTypeSize` approximates?
        _ -> Nothing

    TyBool ->
       case t of
         Right ty | Cry.tIsBit ty -> pure []
         _ -> Nothing

    TyNotBool ->
      case t of
        Right ty | not (Cry.tIsBit ty) -> pure [Right ty]
        _ -> Nothing

    TyAny ->
      case t of
        Right ty -> pure [Right ty]
        _ -> Nothing




-- | Can we instantiate the first type so that it becomes equal to the
-- second one (ignoring size computations).  If the two can't be made to
-- match exactly, but only differ in the representations of sequences,
-- we also return a hint describing the cast from the 2nd argument to
-- the instantiate 1st argument.
matchType :: Type -> Type -> Maybe (RepHint, Subst)
matchType patTy argTy =
  case patTy of
    TPoly x   -> pure (NoHint, suSingleType x argTy)
    TBool     -> guard (argTy == TBool) >> pure (NoHint, suEmpty)
    TInteger  -> guard (argTy == TInteger) >> pure (NoHint, suEmpty)
    TRational -> guard (argTy == TRational) >> pure (NoHint, suEmpty)
    TFloat    -> guard (argTy == TFloat) >> pure (NoHint, suEmpty)
    TDouble   -> guard (argTy == TDouble) >> pure (NoHint, suEmpty)

    TIntegerMod x
      | TIntegerMod y <- argTy ->
          guard (matchSize x y) >> pure (NoHint, suEmpty)
      | otherwise -> Nothing


    TTuple ts
      | TTuple ts1 <- argTy, length ts == length ts1 ->
        do (hints,sus) <- unzip <$> zipWithM matchType ts ts1
           su <- foldM suMergeMaybe suEmpty sus
           let hint = if all isNoHint hints then NoHint else TupHint hints
           pure (hint, su)

      | otherwise -> Nothing

    TFun args res
      | TFun args1 res1 <- argTy ->
        do (hR, suR) <- matchType res res1
           guard (length args == length args1)
           (hAs, suAs) <- unzip <$> zipWithM matchType args args1
           let hint = if all isNoHint (hR : hAs)
                              then NoHint else foldr (:->) hR hAs
           su <- foldM suMergeMaybe suR suAs
           pure (hint, su)

      | otherwise -> Nothing

    TWord x
      | TWord y <- argTy -> guard (matchSize x y) >> pure (NoHint, suEmpty)
      | TStream (IRSize y) elTy <- argTy, matchSize x y ->
        do (_h,su1) <- matchType TBool elTy
           pure (AsWord, su1)
      --No array: we shouldn't have cases for array of boo.?
      | otherwise -> Nothing

    TArray sz elTy

      | TArray sz1 elTy1 <- argTy ->
        guard (matchSize sz sz1) >> matchType elTy elTy1

      | TStream (IRSize sz1) elTy1 <- argTy, matchSize sz sz1 ->
        do (h,su1) <- matchType elTy elTy1
           pure (AsArray h, su1)

      --- No word, we shouldn't have arrays of bool
      | otherwise -> Nothing

    TStream sz elTy
      | TStream sz1 elTy1 <- argTy ->
        guard (matchStreamSize sz sz1) >> matchType elTy elTy1

      | TWord y <- argTy, matchStreamSize sz (IRSize y) ->
        do (_h, su) <- matchType elTy TBool
           pure (AsStream NoHint, su)

      | TArray y elTy1 <- argTy, matchStreamSize sz (IRSize y) ->
        do (h1, su) <- matchType elTy elTy1
           pure (AsStream h1, su)

      | otherwise -> Nothing




