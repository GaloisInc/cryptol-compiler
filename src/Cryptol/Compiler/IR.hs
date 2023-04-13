-- | Basic IR for code generation.
module Cryptol.Compiler.IR
  ( module Cryptol.Compiler.IR
  , module Cryptol.Compiler.IR.Type
  ) where


import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Common
import Cryptol.Compiler.IR.Type

--------------------------------------------------------------------------------
-- Specialization for Cryptol names

-- | Types of functions
type FunType = IRFunType Cry.TParam

-- | Value types, specialized to Cryptol names
type Trait = IRTrait Cry.TParam

-- | Value types, specialized to Cryptol names
type Type = IRType Cry.TParam

-- | Possibly infinite IR size types, specialized to Cryptol names
type StreamSize = IRStreamSize Cry.TParam

-- | Size types, specialized to Cryptol names
type Size = IRSize Cry.TParam

-- | Names, specialized to Cryptol names
type Name = IRName Cry.TParam Cry.Name

-- | Function names, specialized to Cryptol names
type FunName = IRFunName Cry.Name

-- | Declarations, specialized to Cryptol names
type Decl = IRDecl Cry.TParam Cry.Name

-- | Expressions, specialized to Cryptol names
type Expr = IRExpr Cry.TParam Cry.Name

-- | Primitives, specialized to Cryptol names
type Prim = IRExpr Cry.TParam


--------------------------------------------------------------------------------

-- | Function name.  Stores the type of the result.
data IRFunName tname name = IRFunName name FunInstance (IRType tname)

-- | Typed names
data IRName tname name = IRName name (IRType tname)

-- | Declarations
data IRDecl tname name =
    IRFun (IRFunDecl tname name)

-- | A function declaration
data IRFunDecl tname name =
  IRFunDecl
    { irfName       :: IRFunName tname name
    , irfTParams    :: [tname]
    , irfTraits     :: [IRTrait tname]
    , irfSizeParams :: [IRName tname name]
    , irfParams     :: [IRName tname name]
    , irfDef        :: IRFunDef tname name
    }

data IRFunDef tname name =
    IRFunPrim
  | IRFunDef (IRExpr tname name)

-- | Expressions
newtype IRExpr tname name = IRExpr (IRExprF tname name (IRExpr tname name))

-- | The various flavours of expressions.
-- Expressions are split in two parts because this makes it easier to
-- write generic traversals.
data IRExprF tname name expr =
    IRVar (IRName tname name)
  | IRCall (IRFunName tname name) [IRType tname] [expr] [expr]
    -- size args, args
    -- The type of the result is stored in the name.
    -- Note that this should be the type, for this call site (i.e., type
    -- parameters may affect it

  | IRIf expr expr expr
    deriving (Functor,Foldable,Traversable)


--------------------------------------------------------------------------------
-- Computing Types

-- | Things that have a type.
class PP tname => HasType t tname | t -> tname where

  -- | Compute the type of something.
  typeOf :: t -> IRType tname

instance PP tname => HasType (IRName tname name) tname where
  typeOf (IRName _ t) = t

instance PP tname => HasType (IRFunName tname name) tname where
  typeOf (IRFunName _ _ t) = t

instance HasType expr tname => HasType (IRExprF tname name expr) tname where
  typeOf expr =
    case expr  of
      IRVar x         -> typeOf x
      IRCall f _ _ _  -> typeOf f
      -- IRPrim p        -> typeOf p
      IRIf _ x _      -> typeOf x


instance PP tname => HasType (IRExpr tname name) tname where
  typeOf (IRExpr e) = typeOf e

{-
instance HasType e tname => HasType (IRPrim tname e) tname where
  typeOf prim =
    case prim of
      IntegerLit _ t  -> t
      RationalLit _ t -> t
      BoolLit _       -> TBool

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
-}

--------------------------------------------------------------------------------
-- Pretty Printing

instance (PP tname, PP name) => PP (IRName tname name) where
  pp (IRName x t) =
    getPPCfg \cfg ->
      if ppShowTypes cfg
        then parensAfter 0 (pp x <+> ":" <+> withPrec 0 (pp t))
        else pp x

-- XXX: should we print the result type?
instance (PP tname, PP name) => PP (IRFunName tname name) where
  pp (IRFunName x i _)
    | isEmptyInstance i = pp x
    | otherwise         = hcat [ pp x, "@", pp i ]

instance (PP tname, PP name, PP expr) => PP (IRExprF tname name expr) where
  pp expr =
    case expr of
     IRVar x            -> pp x
     IRCall f ts ss es  -> withPrec 0
                         $ hcat [ pp f, targs, args ]
        where
        targs = case ts of
                  [] -> mempty
                  _  -> hcat [ "::<", commaSep (map pp ts), ">" ]
        args = parens (commaSep (map pp (ss ++ es)))

--     IRPrim prim    -> pp prim
     IRIf e1 e2 e3  ->
       parensAfter 0 $
       withPrec 0 $
       vcat [ "if" <+> pp e1
            , nest 2 "then" <+> pp e2
            , nest 2 "else" <+> pp e3
            ]

instance (PP tname, PP name) => PP (IRExpr tname name) where
  pp (IRExpr e) = pp e

instance (PP tname, PP name) => PP (IRDecl tname name) where
  pp (IRFun fd) = pp fd

instance (PP tname, PP name) => PP (IRFunDef tname name) where
  pp def =
    case def of
     IRFunPrim  -> "/* external */"
     IRFunDef e -> pp e

instance (PP tname, PP name) => PP (IRFunDecl tname name) where
  pp fd =
    vcat
      [ "fn" <+> pp (irfName fd) <+> tps <+> args <+> "->" <+> resT <+> "{"
      , nest 2 (pp (irfDef fd))
      , "}"
      ]
    where
    tps = case irfTParams fd of
            [] -> mempty
            ps -> hcat [ "<", commaSep (map pp ps), ">" ]

    args = parens
         $ withTypes
         $ commaSep
         $ map pp
         $ irfSizeParams fd ++ irfParams fd

    resT = withPrec 0 (pp (typeOf (irfName fd)))


