module Cryptol.Compiler.Cry2IR.ChooseSpec where

import Data.Either(partitionEithers)
import Control.Monad(guard,zipWithM,foldM,unless)

import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Subst
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.Cry2IR.RepHints
import Cryptol.Compiler.IR.Cryptol




-- | Check if some concrete type arguments match a particular
-- function instance.  Returns (type arguments, size arguments)
matchFun ::
  (FunName, FunType) {- ^ Instance under consideration -} ->
  [Either StreamSize Cry.Type] {- ^ Type parameters, numeric ones compiled-} ->
  Type {- ^ Target type -} ->
  Maybe ([Cry.Type], [Size])
matchFun (f, funTy) tyArgs tgtT =
  do let FunInstance pinfo = irfnInstance f
     matchPs <- zipWithM matchParamInfo pinfo tyArgs
     let (nums, vals) = partitionEithers (concat matchPs)
     let numPs = ftSizeParams funTy
         valPs = ftTypeParams funTy

     unless (length nums == length numPs) $
        panic "matchFun" ["Numeric argument mismatch"]
     unless (length vals == length valPs) $
        panic "matchFun" ["Value type argument mismatch"]

     let numSu = foldr (\(x,sz) -> suAddSize (irsName x) (IRSize sz)) suEmpty
                       (zip numPs nums)

     let resT = apSubst numSu (ftResult funTy)
     (hint,moreSu) <- matchType resT tgtT
     let totSu = suMerge moreSu numSu

     -- * if hint is empty, then we have an exact match
     -- * the `moreSu` should tell us the representations to be used for type
     -- parameters that are mentioned in the result.
     -- * Any other type parameters we should compile the default way.

     pure (vals, nums)


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

    NumVar _ ->
      case t of
        Left (IRSize s) -> pure [Left s]
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
        do (h,su1) <- matchType TBool elTy
           pure (AsStream h, su1)
      --No array: we shouldn't have cases for array of boo.?
      | otherwise -> Nothing

    TArray sz elTy

      | TArray sz1 elTy1 <- argTy ->
        guard (matchSize sz sz1) >> matchType elTy elTy1

      | TStream (IRSize sz1) elTy1 <- argTy, matchSize sz sz1 ->
        do (h,su1) <- matchType elTy elTy1
           pure (AsStream h, su1)

      --- No word, we shouldn't have arrays of bool
      | otherwise -> Nothing

    TStream sz elTy
      | TStream sz1 elTy1 <- argTy ->
        guard (matchStreamSize sz sz1) >> matchType elTy elTy1

      | TWord y <- argTy, matchStreamSize sz (IRSize y) ->
        do (_h, su) <- matchType elTy TBool
           pure (AsWord, su)

      | TArray y elTy1 <- argTy, matchStreamSize sz (IRSize y) ->
        do (h1, su) <- matchType elTy elTy1
           pure (AsArray h1, su)

      | otherwise -> Nothing




