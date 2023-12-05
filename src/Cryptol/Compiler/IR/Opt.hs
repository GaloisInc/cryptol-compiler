-- | Simple optimizations.
module Cryptol.Compiler.IR.Opt(rewFun) where

import Data.Maybe(fromMaybe)
import Control.Monad(guard)

import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.IR.Cryptol

rewFun :: FunDecl -> FunDecl
rewFun fd =
  fd { irfDef =
         case irfDef fd of
           IRFunDef xs e -> IRFunDef xs (rewExpr e)
           IRFunPrim     -> IRFunPrim
     }

-- | Bottom up rewrites
rewExpr :: Expr -> Expr
rewExpr (IRExpr expr) =
  let rew = IRExpr (rewExpr <$> expr)
  in fromMaybe rew (fuseIndexVector rew)


matchPrimCall ::
  Expr ->
  Maybe (IRPrim, [Type], [(Size,SizeVarSize)], [(Expr,Type)], Type)
matchPrimCall (IRExpr expr) =
  case expr of
    IRCallFun
      IRCall
        { ircFun = IRTopFun
                   IRTopFunCall
                     { irtfName = IRFunName { irfnName = IRPrimName p }
                     , irtfTypeArgs = tyArgs
                     , irtfSizeArgs = szArgs
                     }
        , ircArgs = es
        , ircArgTypes = ets
        , ircResType = resT
        } -> Just (p,tyArgs,szArgs,zip es ets, resT)
    _ -> Nothing


-- toVec xs !! n   ~> xs !! n
fuseIndexVector :: Expr -> Maybe Expr
fuseIndexVector expr =
  do (CryPrim (Cry.PrimIdent mo nm),_ts,_sz,[xs,i],resT) <- matchPrimCall expr
     newP <- case nm of
               "!" -> pure StreamLookupBack
               "@" -> pure StreamLookup
               _   -> Nothing

     guard (mo == Cry.preludeName)

     (prim, _ts1, _sz1,[str], _argResT) <- matchPrimCall (fst xs)

     let newE = callPrim newP [fst str, fst i] resT

     case prim of
        StreamToArray -> Just newE
        StreamToWord  -> Just newE
        _ -> Nothing



