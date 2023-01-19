-- | Basic IR for code generation.
module Cryptol.Compiler.IR where

import Cryptol.Utils.Panic(panic)
import Cryptol.Compiler.PP

-- | Typed names
data IRName name = IRName name IRType

-- | Value types
data IRType =
    TArray         IRSize  IRType
  | TStream (Maybe IRSize) IRType   -- ^ Nothing means infinite
  | TBool
  | TWord          IRSize
  | TTuple [IRType]
    deriving Show

data IRSize =
    IRFixed Int
  | IRUnknown
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
-- Computing Types

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
      WordLit _ n -> TWord (IRFixed n)
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
          TArray  _sz t -> t
          TStream _sz t -> t
          TWord   _sz   -> TBool
          t             -> panic "typeOf" [ "IndexIn", show t ]


--------------------------------------------------------------------------------
-- Pretty Printing


instance PP IRType where
  pp ty =
    case ty of
      TArray sz t -> brackets (pp t <.> szDoc)
        where
        szDoc =
          case sz of
            IRUnknown -> mempty
            IRFixed n -> ";" <+> pp n

      TStream mbsz t -> brackets ("str|" <+> pp t <.> szdoc)
        where
        szdoc =
          case mbsz of
            Nothing -> "; inf"
            Just sz ->
              case sz of
                IRUnknown -> mempty
                IRFixed s -> ";" <+> pp s

      TBool     -> "bool"
      TWord sz  ->
        case sz of
          IRUnknown -> "bitvec"
          IRFixed n -> "u_" <.> pp n
      TTuple ts -> parens (commaSep (map pp ts))


instance PP name => PP (IRName name) where
  pp (IRName x t) =
    getPPCfg \cfg ->
      if ppShowTypes cfg
        then parensAfter 0 (pp x <+> ":" <+> pp t)
        else pp x

instance (PP name, PP expr) => PP (IRExprF name expr) where
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

instance PP name => PP (IRExpr name) where
  pp (IRExpr e) = pp e

instance PP expr => PP (IRPrim expr) where
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
        | null es   -> parensAfter 0 ("[] :" <+> pp (TArray (IRFixed 0) t))
        | otherwise -> withPrec 0 (brackets (commaSep (map pp es)))

      IndexIn a i -> withPrec 1 (pp a) <.> brackets (withPrec 0 (pp i))

    where
    ppInfix n l r op e1 e2 =
      parensAfter n (withPrec l (pp e1) <+> op <+> withPrec r (pp e2))

