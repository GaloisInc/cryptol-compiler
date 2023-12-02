-- | Change the identifiers.
module Cryptol.Compiler.IR.Rename where

import Cryptol.Compiler.IR

-- | Traverse names using the given functions.
rename ::
  (Applicative m, Rename f) =>
  (ta -> m tb) -> (a -> m b) -> f ta a -> m (f tb b)
rename renT ren =
  let ?renameT = renT
      ?rename  = ren
  in renameIP

type RenFuns f ta a tb b =
  ( ?renameT :: ta -> f tb
  , ?rename  ::  a -> f b
  )

class Rename f where
  renameIP :: (Applicative m, RenFuns m ta a tb b) => f ta a -> m (f tb b)

class RenameF f where
  frenameIP ::
    (Applicative m, RenFuns m ta a tb b) => f ta a (m x) -> m (f tb b x)

instance Rename IRName where
  renameIP (IRName x t) = IRName <$> ?rename x <*> traverse ?renameT t

instance Rename IRFunDecl where
  renameIP fd =
    mk <$> traverse ?rename (irfName fd)
       <*> traverse ?renameT (irfType fd)
       <*> renameIP (irfDef fd)
    where
    mk a b c = IRFunDecl { irfName = a, irfType = b, irfDef = c }

instance Rename IRFunDef where
  renameIP def =
    case def of
      IRFunPrim     -> pure IRFunPrim
      IRFunDef xs e ->
        IRFunDef <$> traverse ?rename xs <*> renameIP e

instance Rename IRExpr where
  renameIP (IRExpr expr) = IRExpr <$> frenameIP (renameIP <$> expr)

instance RenameF IRExprF where
  frenameIP expr =
    case expr of
      IRVar x       -> IRVar <$> renameIP x
      IRCallFun x   -> IRCallFun <$> frenameIP x
      IRClosure x   -> IRClosure <$> frenameIP x
      IRLam xs e    -> IRLam <$> traverse renameIP xs <*> e
      IRIf e1 e2 e3 -> IRIf <$> e1 <*> e2 <*> e3
      IRLet x e1 e2 -> IRLet <$> renameIP x <*> e1 <*> e2
      IRStream s    -> IRStream <$> frenameIP s

instance RenameF IRStreamExpr where
  frenameIP expr =
    IRStreamExpr
      <$> traverse ?renameT (irsType expr)
      <*> traverse renExt (irsExterns expr)
      <*> sequenceA (irsRec expr)
      <*> irsNext expr
    where
    renExt (x,e) = (,) <$> renameIP x <*> e

instance RenameF IRCall where
  frenameIP c =
    IRCall
      <$> frenameIP (ircFun c)
      <*> traverse ?renameT (ircFunType c)
      <*> traverse (traverse ?renameT) (ircArgTypes c)
      <*> traverse ?renameT (ircResType c)
      <*> sequenceA (ircArgs c)

instance RenameF IRCallable where
  frenameIP call =
    case call of
      IRTopFun f -> IRTopFun <$> renameIP f
      IRFunVal e -> IRFunVal <$> e

instance Rename IRTopFunCall where
  renameIP c =
    IRTopFunCall
      <$> traverse ?rename (irtfName c)
      <*> traverse (traverse ?renameT) (irtfTypeArgs c)
      <*> traverse szArg (irtfSizeArgs c)
    where
    szArg (x,s) = (\x' -> (x',s)) <$> traverse ?renameT x

