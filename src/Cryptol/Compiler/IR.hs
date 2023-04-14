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
type FunName = IRFunName Cry.TParam Cry.Name

-- | Declarations, specialized to Cryptol names
type FunDecl = IRFunDecl Cry.TParam Cry.Name

-- | Expressions, specialized to Cryptol names
type Expr = IRExpr Cry.TParam Cry.Name

-- | Primitives, specialized to Cryptol names
type Prim = IRExpr Cry.TParam


--------------------------------------------------------------------------------

-- | Function name.  Stores the type of the result.
data IRFunName tname name = IRFunName
  { irfnName     :: name
  , irfnInstance :: FunInstance
  , irfnResult   :: IRType tname
  }

-- | Typed names
data IRName tname name = IRName name (IRType tname)

-- | A function declaration
data IRFunDecl tname name =
  IRFunDecl
    { irfName       :: IRFunName tname name
    , irfTParams    :: [tname]
    , irfTraits     :: [IRTrait tname]
    , irfSizeParams :: [IRSizeName tname]
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
    IRVar     (IRName tname name)

  | IRCall (IRFunName tname name)       -- function to call
           [IRType tname]               -- type arguments
           [(IRSize tname,SizeVarSize)] -- size type arugments
           [expr]                       -- normal arguments
    -- size args, args
    -- The type of the result is stored in the name.
    -- Note that this should be the type, for this call site (i.e., type
    -- parameters may affect it

  | IRIf expr expr expr

    deriving (Functor,Foldable,Traversable)

--------------------------------------------------------------------------------
-- Computing Types

type instance TName (IRFunName tname name) = tname
type instance TName (IRName tname name) = tname
type instance TName (IRFunDecl tname name) = tname
type instance TName (IRFunDef tname name) = tname
type instance TName (IRExpr tname name) = tname
type instance TName (IRExprF tname name expr) = tname


-- | Things that have a type.
class HasType t where
  -- | Compute the type of something.
  typeOf :: t -> IRType (TName t)

instance HasType (IRName tname name) where
  typeOf (IRName _ t) = t

instance HasType (IRSizeName tname) where
  typeOf (IRSizeName _ t) = sizeVarSizeType t

instance HasType (IRFunName tname name) where
  typeOf (IRFunName _ _ t) = t

instance (HasType expr, TName expr ~ tname) =>
  HasType (IRExprF tname name expr) where
  typeOf expr =
    case expr  of
      IRVar x         -> typeOf x
      IRCall f _ _ _  -> typeOf f
      IRIf _ x _      -> typeOf x

instance HasType (IRExpr tname name) where
  typeOf (IRExpr e) = typeOf e


funDeclType :: IRFunDecl tname name -> IRFunType tname
funDeclType fd =
  IRFunType
    { ftTypeParams = irfTParams fd
    , ftTraits     = irfTraits fd
    , ftSizeParams = irfSizeParams fd
    , ftParams     = map typeOf (irfParams fd)
    , ftResult     = typeOf (irfName fd)
    }



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
      IRVar x -> pp x

      IRCall f ts ss es  -> withPrec 0
                          $ hcat [ pp f, targs, args ]
         where
         targs = case ts of
                   [] -> mempty
                   _  -> hcat [ "::<", commaSep (map pp ts), ">" ]
         args = parens (commaSep (map ppSize ss ++ map pp es))
         ppSize (x,s) = pp x <+> "as" <+> pp (sizeVarSizeTypeFor x s)

      IRIf e1 e2 e3  ->
        parensAfter 0 $
        withPrec 0 $
        vcat [ "if" <+> pp e1
             , nest 2 "then" <+> pp e2
             , nest 2 "else" <+> pp e3
             ]


instance (PP tname, PP name) => PP (IRExpr tname name) where
  pp (IRExpr e) = pp e

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
         $ map pp (irfSizeParams fd) ++ map pp (irfParams fd)

    resT = withPrec 0 (pp (typeOf (irfName fd)))


