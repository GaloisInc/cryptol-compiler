-- | Change the identifiers.
module Cryptol.Compiler.IR.Rename where

import Cryptol.Compiler.IR

rename :: Rename f => (ta -> tb) -> (a -> b) -> f ta a -> f tb b
rename renT ren =
  let ?renameT = renT
      ?rename  = ren
  in renameIP

type RenFuns ta a tb b = 
  ( ?renameT :: ta -> tb
  , ?rename  ::  a ->  b
  )

class Rename f where
  renameIP :: RenFuns ta a tb b => f ta a -> f tb b

class RenameF f where
  frenameIP :: RenFuns ta a tb b => f ta a x -> f tb b x

instance Rename IRName where
  renameIP (IRName x t) = IRName (?rename x) (?renameT <$> t)

instance Rename IRFunDecl where
  renameIP fd =
    IRFunDecl
      { irfName = ?rename <$> irfName fd
      , irfType = ?renameT <$> irfType fd
      , irfDef  = renameIP (irfDef fd)
      }

instance Rename IRFunDef where
  renameIP def =
    case def of
      IRFunPrim     -> IRFunPrim
      IRFunDef xs e -> IRFunDef (map ?rename xs) (renameIP e)

instance Rename IRExpr where
  renameIP (IRExpr expr) = IRExpr (frenameIP (renameIP <$> expr))

instance RenameF IRExprF where
  frenameIP expr =
    case expr of
      IRVar x       -> IRVar (renameIP x)
      IRCallFun x   -> IRCallFun (frenameIP x)
      IRClosure x   -> IRClosure (frenameIP x)
      IRLam xs e    -> IRLam (map renameIP xs) e
      IRIf e1 e2 e3 -> IRIf e1 e2 e3
      IRLet x e1 e2 -> IRLet (renameIP x) e1 e2

instance RenameF IRCall where
  frenameIP c =
    IRCall
      { ircFun      = frenameIP (ircFun c)
      , ircArgTypes = [ ?renameT <$> t | t <- ircArgTypes c ]
      , ircResType  = ?renameT <$> ircResType c
      , ircArgs     = ircArgs c
      }

instance RenameF IRCallable where
  frenameIP call =
    case call of
      IRTopFun f -> IRTopFun (renameIP f)
      IRFunVal e -> IRFunVal e

instance Rename IRTopFunCall where
  renameIP c =
    IRTopFunCall
      { irtfName      = ?rename <$> irtfName c
      , irtfTypeArgs  = [ ?renameT <$> t | t <- irtfTypeArgs c ]
      , irtfSizeArgs  = [ (?renameT <$> x,s) | (x,s) <- irtfSizeArgs c ]
      }


