module Cryptol.Compiler.IR where

import Cryptol.Utils.Panic(panic)

data Name = Name String IRType

data IRType =
    TArray IRType
  | TStream IRType
  | TBool
  | TWord Int
  | TTuple [IRType]
    deriving Show

data IRDecl =
  IRFun Name [Name] IRExpr

data IRExprF e =
    IRVar Name
  | IRCall Name [e]
  | IRPrim (IRPrim e)
  | IRIf e e e
    deriving (Functor,Foldable,Traversable)

newtype IRExpr = IRExpr (IRExprF IRExpr)

data IRPrim e =
    WordLit Integer Int   -- ^ value, size
  | BoolLit Bool

  | Add e e
  | Sub e e
  | Mul e e
  | Div e e
  | Mod e e

  | Tuple [e]
  | Select e Int

  | Array IRType [e]
  | IndexIn e e           -- ^ array, index
    deriving (Functor,Foldable,Traversable)



--------------------------------------------------------------------------------
class HasType t where
  typeOf :: t -> IRType

instance HasType Name where
  typeOf (Name _ t) = t

instance HasType e => HasType (IRExprF e) where
  typeOf expr =
    case expr  of
      IRVar x     -> typeOf x
      IRCall f _  -> typeOf f
      IRPrim p    -> typeOf p
      IRIf _ x _  -> typeOf x

instance HasType IRExpr where
  typeOf (IRExpr e) = typeOf e

instance HasType e => HasType (IRPrim e) where
  typeOf prim =
    case prim of
      WordLit _ n -> TWord n
      BoolLit _   -> TBool

      Add x _ -> typeOf x
      Sub x _ -> typeOf x
      Mul x _ -> typeOf x
      Div x _ -> typeOf x
      Mod x _ -> typeOf x

      Tuple es -> TTuple (map typeOf es)

      Select e n ->
        case typeOf e of
          TTuple ts
            | t : _ <- drop n ts -> t
          t                      -> panic "typeOf" [ "Select", show t]

      Array t _ -> t

      IndexIn arr _ ->
        case typeOf arr of
          TArray t  -> t
          TStream t -> t
          t         -> panic "typeOf" [ "IndexIn", show t ]



