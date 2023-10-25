-- | Eta expand definitions
module Cryptol.IR.Eta (etaModule) where

import Data.Map(Map)
import qualified Data.Map as Map

import MonadLib

import Cryptol.Utils.Panic(panic)
import Cryptol.Utils.Ident(packIdent,Namespace(NSValue))
import Cryptol.Parser.Position(emptyRange)
import Cryptol.ModuleSystem.Name(mkLocal,Supply)
import Cryptol.TypeCheck.TypeOf(fastTypeOf)
import Cryptol.TypeCheck.Type
import Cryptol.TypeCheck.AST

type Env = Map Name Schema
type M   = ReaderT Env (StateT Supply Id)

newName :: M Name
newName = sets (mkLocal NSValue i r)
  where i = packIdent "x" -- ?
        r = emptyRange -- ?

exprType :: Expr -> M Type
exprType e =
  do env <- ask
     pure (fastTypeOf env e)

withLocals :: Env -> M a -> M a
withLocals env = mapReader (Map.union env)


--------------------------------------------------------------------------------
toFun :: Type -> ([Type], Type)
toFun ty =
  case tIsFun ty of
    Just (t,ts) -> (t:as,b)
      where (as,b) = toFun ts
    Nothing -> ([],ty)

arity :: Type -> [Type]
arity = fst . toFun

--------------------------------------------------------------------------------

etaModule ::
  Map Name Schema -> Supply -> ModuleG mname -> (ModuleG mname, Supply)
etaModule env su m = (m { mDecls = d }, su')
  where
  ((_,d),su') =
    runId $
    runStateT su $
    runReaderT env $
    etaDeclGroups (mDecls m)

etaDeclGroups :: [DeclGroup] -> M (Env, [DeclGroup])
etaDeclGroups ds =
  case ds of
    [] -> pure (mempty, [])
    dg : more ->
      do (env,dg')    <- etaDeclGroup dg
         (env',more') <- withLocals env (etaDeclGroups more)
         pure (Map.union env env', dg' : more')

etaDeclGroup :: DeclGroup -> M (Env, DeclGroup)
etaDeclGroup dg =
  case dg of
    Recursive ds ->
      do let env = Map.fromList (map sig ds)
         ds' <- withLocals env (traverse etaDecl ds)
         pure (env, Recursive ds')
    NonRecursive d ->
      do d' <- NonRecursive <$> etaDecl d
         let env = uncurry Map.singleton (sig d)
         pure (env, d')

sig :: Decl -> (Name, Schema)
sig d = (dName d, dSignature d)

etaDecl :: Decl -> M Decl
etaDecl d =
  case dDefinition d of
    DPrim {} -> pure d
    DForeign {} -> pure d
    DExpr e ->
      do let (tps, e1) = splitWhile splitTAbs e
             (pps, e2) = splitWhile splitProofAbs e1
         newFun <- etaExprT (sType (dSignature d)) e2
         let withProp = foldr EProofAbs newFun pps
         pure d { dDefinition = DExpr (foldr ETAbs withProp tps) }

etaExpr' :: Expr -> M Expr
etaExpr' e =
  do t <- exprType e
     etaExprT t e

etaExprT :: Type -> Expr -> M Expr
etaExprT ty e =
  do let (ps,e') = splitWhile splitAbs e
     args <- forM (drop (length ps) (arity ty)) \t ->
               do x <- newName
                  pure (x,t)
     let newArgs = map (EVar . fst) args
         allArgs = ps ++ args
         env = Map.fromList [ (x, tMono t) | (x,t) <- allArgs ]
     e'' <- withLocals env (etaExpr newArgs e')
     pure (foldr (uncurry EAbs) e'' allArgs)


etaExpr :: [Expr] -> Expr -> M Expr
etaExpr args expr =
  case expr of

    EList es elT ->
      do noArgs
         (`EList` elT) <$> traverse (etaExprT elT) es

    ETuple es ->
      do noArgs
         ETuple <$> traverse etaExpr' es

    ERec fs ->
      do noArgs
         ERec <$> traverse etaExpr' fs

    ESet t re l e ->
      do noArgs
         re' <- etaExprT t re
         e'  <- etaExpr' e
         pure (ESet t re' l e')

    EComp len elT e mms ->
      do noArgs
         (env,mms') <- etaMatchess mms
         e' <- withLocals env (etaExprT elT e)
         pure (EComp len elT e' mms')

    ESel e s ->
      do e' <- etaExpr' e
         pure (doApp (ESel e' s))

    -- Assume that we only instantiate variables in these casees
    EVar {}       -> pure (doApp expr)
    ETApp {}      -> pure (doApp expr)
    EProofApp {}  -> pure (doApp expr)

    EApp e1 e2 ->
      do e2' <- etaExpr' e2
         etaExpr (e2' : args) e1

    EIf e1 e2 e3 ->
      do e1'  <- etaExprT tBit e1
         e2'  <- etaExpr args e2
         e3'  <- etaExpr args e3
         pure (EIf e1' e2' e3')

    EAbs x t e ->
      withLocals (Map.singleton x (tMono t))
      case args of
        [] -> EAbs x t <$> etaExpr' e
        a : as ->
          do e' <- etaExpr as e
             let d = Decl { dName  = x
                          , dSignature = tMono t
                          , dDefinition = DExpr a
                          , dPragmas = []
                          , dInfix = False
                          , dFixity = Nothing
                          , dDoc = Nothing
                          }
             pure (EWhere e' [NonRecursive d])

    ETAbs {}     -> panic "etaExpr" ["Unexpected ETAbs"]
    EProofAbs {} -> panic "etaExpr" ["Unexpected EProofAbs"]

    ELocated r e  -> ELocated r <$> etaExpr args e

    EWhere e ds ->
      do (env,ds') <- etaDeclGroups ds
         e' <- withLocals env (etaExpr args e)
         pure (EWhere e' ds')

    EPropGuards alts t ->
      do alts' <- forM alts \(ps,e) -> (,) ps <$> etaExpr args e
         let (as,b) = toFun t
             as' = drop (length args) as
         pure (EPropGuards alts' (foldr tFun b as'))
  where
  noArgs  = unless (null args) (panic "etaExpr" ["Unexpected arguments"])
  doApp e = foldl EApp e args

etaMatchess :: [[Match]] -> M (Env, [[Match]])
etaMatchess mss =
  do (envs, mss') <- mapAndUnzipM etaMatches mss
     pure (Map.unions envs, mss')

-- In sequence
etaMatches :: [Match] -> M (Env, [Match])
etaMatches ms =
  case ms of
    [] -> pure (mempty, [])
    m : more ->
      do (env,m') <- etaMatch m
         (env', ms') <- withLocals env (etaMatches more)
         pure (Map.union env env', m' : ms')

etaMatch :: Match -> M (Env,Match)
etaMatch ma =
  case ma of
    From x len elT e ->
      do let ty = tSeq len elT
         e' <- etaExprT ty e
         pure (Map.singleton x (tMono ty), From x len elT e')
    Let d ->
      do let (x,t) = sig d
         d' <- etaDecl d
         pure (Map.singleton x t, Let d')


