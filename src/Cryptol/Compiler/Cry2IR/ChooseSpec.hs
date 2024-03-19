module Cryptol.Compiler.Cry2IR.ChooseSpec (selectInstance) where

import Data.Set(Set)
import Data.Set qualified as Set
import Data.List(transpose)
import Data.Either(partitionEithers)
import MonadLib

import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry
import Cryptol.TypeCheck.Subst qualified as Cry

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
  do instDB <- doCryC (M.getFun f)
     let is = instanceMapToList instDB
         forceMemSize = findMemSizeParams (map (irfnInstance . M.fiName) is)
     targs <- zipWithM prepTArg forceMemSize tyArgs
     select is Nothing targs is
  where
  select is0 candidate targs is =
    case is of
      [] ->
        case candidate of
          Nothing ->
            unsupported $ vcat $
               [ "Unsupported function instance:"
               , "Function:" <+> pp f
               , "Type arguments:" <+>
                    withTypes (commaSep (map (cryPP . fst) targs))
               , "Target type:" <+> pp tgtT
               , "Available instances:"
               ] ++ [ "  " <+> (pp (M.fiName fi) <+> "::"
                                    $$ nest 2 (pp (M.fiType fi)))
                    | fi <- is0 ]
          Just ans -> doSelect ans
      i : more ->
        do mbYes <- matchFun i targs tgtT
           case mbYes of
             Nothing -> select is0 candidate targs more
             Just this@(_,_,hint) ->
               case hint of
                 NoHint -> doSelect this
                 _ ->
                    let newCandidate =
                          case candidate of
                            Nothing     -> this
                            Just other  -> pickBetter this other
                    in select is0 (Just newCandidate) targs more

  doSelect (c,sm,_) =
    do mapM_ assumeSmall sm
       pure c

  prepTArg mbSz t =
    do trans <-
         case Cry.kindOf t of
           Cry.KNum ->
             case mbSz of
               Just force -> Just <$> compileStreamSizeType force t
               Nothing    -> panic "selectInstance.preTArg" [ "Expected Just?"]
           _ -> pure Nothing
       pure (t, trans)

  pickBetter (h1,sm1,c1) (h2,sm2,c2) =
    if betterHint h1 h2 then (h1,sm1,c1) else (h2,sm2,c2)

  -- XXX: ????
  betterHint _ _ = False  -- we alwyas pick the first one.


-- | Find type parameters that must be compiled using MemSize, because
-- this what the function we are trying to call supports.
findMemSizeParams :: [ FunInstance ] -> [ Maybe Bool ]
findMemSizeParams fis =
  map shouldForceSmall (transpose [ pis | FunInstance pis <- fis ])
  where
  shouldForceSmall xs =
    case xs of
      [] -> Just True
      x : more ->
        case x of
          NumFixed sz ->
            case sz of
              Cry.Inf -> shouldForceSmall more
              Cry.Nat n
                | n >= maxSizeVal -> Just False
                | otherwise       -> shouldForceSmall more
          NumVar sz ->
            case sz of
              LargeSize -> Just False     -- Large size is an option
              MemSize   -> shouldForceSmall more

          -- Not numeric
          TyBool        -> Nothing
          TyNotBool     -> Nothing
          TyAny         -> Nothing


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
  M.FunIface                     {- ^ Instance under consideration -} ->
  [(Cry.Type, Maybe StreamSize)] {- ^ Type parameters, numeric ones compiled-} ->
  Type                           {- ^ Target type -} ->
  ConvertM (Maybe (Call, [Cry.Type], RepHint))
  -- ^ Nothing if the instnace does not apply.
  -- Just (call,hint) if we can use the instance, the hint gives transformation
  -- we have to apply to the result if we use this instance.

matchFun fi tyArgs tgtT =
  runMatch
  do let f = M.fiName fi
         funTy = M.fiType fi
     let FunInstance pinfo = irfnInstance f
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

     let asmps = M.fiSmall fi
         crySu = Cry.listParamSubst (zip (M.asForall asmps) (map fst tyArgs))
     let sm = map (Cry.apSubst crySu) (M.asSizes asmps)
     pure (call, sm, hint)

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
  (Cry.Type, Maybe StreamSize) ->
  Maybe [Either Size Cry.Type]
matchParamInfo pinfo (t,mbSz) =
  case pinfo of

    NumFixed Cry.Inf ->
      case mbSz of
        Just IRInfSize -> pure []
        _ -> Nothing

    NumFixed (Cry.Nat n) ->
      case mbSz of
        Just (IRSize (IRFixedSize m)) | m == n -> pure []
        _ -> Nothing

    NumVar need ->
      case mbSz of
        Just (IRSize s) ->
          let haveSize = sizeTypeSize s
          in if need == haveSize then pure [Left s] else Nothing
        _ -> Nothing

    TyBool
      | Nothing <- mbSz, Cry.tIsBit t -> pure []
      | otherwise    -> Nothing

    TyNotBool
      | Nothing <- mbSz, not (Cry.tIsBit t) -> pure [Right t]
      | otherwise -> Nothing

    TyAny
      | Nothing <- mbSz -> pure [Right t]
      | otherwise -> Nothing




-- | Can we instantiate the first type so that it becomes equal to the
-- second one (ignoring size computations).  If the two can't be made to
-- match exactly, but only differ in the representations of sequences,
-- we also return a hint describing the cast from the 2nd argument to
-- the instantiated 1st argument.
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




