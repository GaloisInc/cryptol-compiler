-- | Basic IR for code generation.
module Cryptol.Compiler.IR where

import Cryptol.Utils.Panic(panic)

-- | Typed names
data IRName name = IRName name IRType

-- | Value types
data IRType =
    TArray IRType
  | TStream IRType
  | TBool
  | TWord Int
  | TTuple [IRType]
    deriving Show

-- | Declarations
data IRDecl name =
  IRFun (IRName name) [IRName name] (IRExpr name)

-- | Expressions
newtype IRExpr name = IRExpr (IRExprF name (IRExpr name))

-- | The various flavours of expressions.
-- Expressions are split in two parse, as this makes it easier to traverse them.
data IRExprF name expr =
    IRVar (IRName name)
  | IRCall (IRName name) [expr]
  | IRPrim (IRPrim expr)
  | IRIf expr expr expr
    deriving (Functor,Foldable,Traversable)


-- | Primitives.
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
-- HasType

-- | Things that have a type.
class HasType t where

  -- | Compute the type of something.
  typeOf :: t -> IRType

instance HasType (IRName name) where
  typeOf (IRName _ t) = t

instance HasType expr => HasType (IRExprF name expr) where
  typeOf expr =
    case expr  of
      IRVar x     -> typeOf x
      IRCall f _  -> typeOf f
      IRPrim p    -> typeOf p
      IRIf _ x _  -> typeOf x

instance HasType (IRExpr name) where
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



