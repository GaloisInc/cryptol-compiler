-- | Basic IR for code generation.
module Cryptol.Compiler.IR
  ( module Cryptol.Compiler.IR
  , module Cryptol.Compiler.IR.Type
  ) where

import Cryptol.Utils.Panic(panic)
import qualified Cryptol.TypeCheck.AST as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Type

--------------------------------------------------------------------------------
-- Specialization for Cryptol names

-- | Value types, specialized to Cryptol names
type Type = IRType Cry.TParam

-- | Possibly infinite IR size types, specialized to Cryptol names
type StreamSize = IRStreamSize Cry.TParam

-- | Size types, specialized to Cryptol names
type Size = IRSize Cry.TParam

-- | Names, specialized to Cryptol names
type Name = IRName Cry.TParam Cry.Name

-- | Declarations, specialized to Cryptol names
type Decl = IRDecl Cry.TParam Cry.Name

-- | Expressions, specialized to Cryptol names
type Expr = IRExpr Cry.TParam Cry.Name

-- | Primitives, specialized to Cryptol names
type Prim = IRExpr Cry.TParam


--------------------------------------------------------------------------------


-- | Typed names
data IRName tname name = IRName name (IRType tname)

-- | Declarations
data IRDecl tname name =
    IRFun (IRName tname name) [IRName tname name] (IRExpr tname name)
    -- ^ The function's result type is in the function's name.


-- | Expressions
newtype IRExpr tname name = IRExpr (IRExprF tname name (IRExpr tname name))

-- | The various flavours of expressions.
-- Expressions are split in two parts because this makes it easier to
-- write generic traversals.
data IRExprF tname name expr =
    IRVar (IRName tname name)
  | IRCall (IRName tname name) [expr]
  | IRPrim (IRPrim tname expr)
  | IRIf expr expr expr
    deriving (Functor,Foldable,Traversable)


-- | Primitives.
data IRPrim tname e =
    WordLit Integer (IRSize tname)
  | BoolLit Bool

  | Add e e
  | Sub e e
  | Mul e e
  | Div e e
  | Mod e e

  | Tuple [e]
  | Select e Int

  | Array (IRType tname) [e]
  | IndexIn e e           -- ^ array, index
    deriving (Functor,Foldable,Traversable)



--------------------------------------------------------------------------------
-- Computing Types

-- | Things that have a type.
class PP tname => HasType t tname | t -> tname where

  -- | Compute the type of something.
  typeOf :: t -> IRType tname

instance PP tname => HasType (IRName tname name) tname where
  typeOf (IRName _ t) = t

instance HasType expr tname => HasType (IRExprF tname name expr) tname where
  typeOf expr =
    case expr  of
      IRVar x     -> typeOf x
      IRCall f _  -> typeOf f
      IRPrim p    -> typeOf p
      IRIf _ x _  -> typeOf x

instance PP tname => HasType (IRExpr tname name) tname where
  typeOf (IRExpr e) = typeOf e

instance HasType e tname => HasType (IRPrim tname e) tname where
  typeOf prim =
    case prim of
      WordLit _ sz  -> TWord sz
      BoolLit _     -> TBool

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
          t -> panic "typeOf" [ "Select", show (pp t) ]

      Array t _ -> t

      IndexIn arr _ ->
        case typeOf arr of
          TArray  _sz t -> t
          TStream _sz t -> t
          TWord   _sz   -> TBool
          t             -> panic "typeOf" [ "IndexIn", show (pp t) ]


--------------------------------------------------------------------------------
-- Pretty Printing

instance (PP tname, PP name) => PP (IRName tname name) where
  pp (IRName x t) =
    getPPCfg \cfg ->
      if ppShowTypes cfg
        then parensAfter 0 (pp x <+> ":" <+> pp t)
        else pp x

instance (PP tname, PP name, PP expr) => PP (IRExprF tname name expr) where
  pp expr =
    case expr of
     IRVar x        -> pp x
     IRCall f es    -> pp f <.> withPrec 0 (parens (commaSep (map pp es)))
     IRPrim prim    -> pp prim
     IRIf e1 e2 e3  ->
       parensAfter 0 $
       withPrec 0 $
       vcat [ "if" <+> pp e1
            , nest 2 "then" <+> pp e2
            , nest 2 "else" <+> pp e3
            ]

instance (PP tname, PP name) => PP (IRExpr tname name) where
  pp (IRExpr e) = pp e

instance (PP tname, PP expr) => PP (IRPrim tname expr) where
  pp prim =
    case prim of
      WordLit n w -> pp n <.> "_u" <.> pp w
      BoolLit b   -> if b then "true" else "false"

      Add e1 e2   -> ppInfix 1 1 1 "+" e1 e2
      Sub e1 e2   -> ppInfix 1 1 2 "-" e1 e2
      Mul e1 e2   -> ppInfix 2 2 2 "*" e1 e2
      Div e1 e2   -> ppInfix 2 2 3 "/" e1 e2
      Mod e1 e2   -> ppInfix 2 2 3 "%" e1 e2

      Tuple es    -> withPrec 0 (commaSep (map pp es))
      Select e n  -> withPrec 1 (pp e) <.> pp n

      Array t es
        | null es -> parensAfter 0 ("[] :" <+> pp (TArray (IRFixedSize 0) t))
        | otherwise -> withPrec 0 (brackets (commaSep (map pp es)))

      IndexIn a i -> withPrec 1 (pp a) <.> brackets (withPrec 0 (pp i))

    where
    ppInfix n l r op e1 e2 =
      parensAfter n (withPrec l (pp e1) <+> op <+> withPrec r (pp e2))

