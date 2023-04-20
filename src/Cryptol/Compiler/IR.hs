-- | Basic IR for code generation.
module Cryptol.Compiler.IR
  ( module Cryptol.Compiler.IR
  , module Cryptol.Compiler.IR.Type
  , module Cryptol.Compiler.IR.Prims
  ) where


import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Common
import Cryptol.Compiler.IR.Type
import Cryptol.Compiler.IR.Prims

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

-- | Various types of names, specialize to Cryptol names
type FunNameFlavor = IRFunNameFlavor Cry.Name

-- | Declarations, specialized to Cryptol names
type FunDecl = IRFunDecl Cry.TParam Cry.Name

-- | Expressions, specialized to Cryptol names
type Expr = IRExpr Cry.TParam Cry.Name

--------------------------------------------------------------------------------

-- | Function name, paired with an instance.
data IRFunName name = IRFunName
  { irfnName     :: IRFunNameFlavor name
  , irfnInstance :: FunInstance
  }

-- | Various types of function names
data IRFunNameFlavor name =
    IRPrimName IRPrim                 -- ^ An IR primtive
  | IRCryPrimName Cry.PrimIdent       -- ^ A Cryptol primitve
  | IRDeclaredFunName name            -- ^ A declared function
    deriving (Eq,Ord)

-- | Typed names
data IRName tname name = IRName name (IRType tname)

-- | A function declaration
data IRFunDecl tname name =
  IRFunDecl
    { irfName       :: IRFunName name
    , irfType       :: IRFunType tname
    , irfDef        :: IRFunDef tname name
    }

-- | The definition of a top-level function.
data IRFunDef tname name =
    IRFunPrim
  | IRFunDef [name] (IRExpr tname name)

-- | Expressions
newtype IRExpr tname name = IRExpr (IRExprF tname name (IRExpr tname name))

-- | The various flavours of expressions.
-- Expressions are split in two parts because this makes it easier to
-- write generic traversals.
data IRExprF tname name expr =
    IRVar     (IRName tname name)
  | IRCallFun (IRCall tname name expr)    -- ^ Call a function
  | IRClosure (IRCall tname name expr)    -- ^ Make a closure
  | IRIf expr expr expr
  | IRTuple [expr]
  | IRLet (IRName tname name) expr expr


-- | Something that can be called.
data IRCallable tname name expr =
    IRTopFun (IRTopFunCall tname name)    -- ^ A top level declaration
  | IRFunVal expr                         -- ^ A closure value

-- | A call to a top level function, containing common parameters
-- that are always present.
data IRTopFunCall tname name =
  IRTopFunCall
    { irtfName      :: IRFunName name
    , irtfTypeArgs  :: [IRType tname]               -- ^ Type arguments
    , irtfSizeArgs  :: [(IRSize tname,SizeVarSize)] -- ^ Size arguments
    }

-- | Information about a function call or a closure.
data IRCall tname name expr =
  IRCall
    { ircFun      :: IRCallable tname name expr  -- ^ What we are calling
    , ircType     :: IRType tname     -- ^ Result of function, or closure type
    , ircArgs     :: [expr]           -- ^ Available arguments
    }


--------------------------------------------------------------------------------
-- Computing Types

type instance TName (IRName tname name) = tname
type instance TName (IRFunDecl tname name) = tname
type instance TName (IRFunDef tname name) = tname
type instance TName (IRExpr tname name) = tname
type instance TName (IRExprF tname name expr) = tname
type instance TName (IRCall tname name expr) = tname
type instance TName (IRTopFunCall tname name) = tname
type instance TName (IRCallable tname name expr) = tname


-- | Things that have a type.
class HasType t where
  -- | Compute the type of something.
  typeOf :: t -> IRType (TName t)

instance HasType (IRName tname name) where
  typeOf (IRName _ t) = t

instance HasType (IRSizeName tname) where
  typeOf (IRSizeName _ t) = sizeVarSizeType t

instance (HasType expr, TName expr ~ tname) =>
  HasType (IRExprF tname name expr) where
  typeOf expr =
    case expr  of
      IRVar x           -> typeOf x
      IRCallFun f       -> typeOf f
      IRClosure f       -> typeOf f
      IRIf _ x _        -> typeOf x
      IRTuple es        -> TTuple (map typeOf es)
      IRLet _ _ e       -> typeOf e

instance HasType (IRCall tname name expr) where
  typeOf = ircType

instance HasType (IRExpr tname name) where
  typeOf (IRExpr e) = typeOf e



--------------------------------------------------------------------------------
-- Pretty Printing

instance (PP tname, PP name) => PP (IRName tname name) where
  pp (IRName x t) =
    getPPCfg \cfg ->
      if ppShowTypes cfg
        then parensAfter 0 (pp x <+> ":" <+> withPrec 0 (pp t))
        else pp x

instance PP name => PP (IRFunNameFlavor name) where
  pp nm =
    case nm of
      IRPrimName p        -> "IR::" <.> pp p
      IRCryPrimName p     -> "CRY::" <.> pp p
      IRDeclaredFunName x -> pp x

instance (PP name) => PP (IRFunName name) where
  pp (IRFunName x i)
    | isEmptyInstance i = pp x
    | otherwise         = hcat [ pp x, "@", pp i ]

ppIRTopFunCall :: (PP tname, PP name) => [Doc] -> IRTopFunCall tname name -> Doc
ppIRTopFunCall extraArgs call =
  withPrec 0 $ hcat [ pp (irtfName call), targs, args ]
  where
  targs = case irtfTypeArgs call of
            [] -> mempty
            ts -> hcat [ "::<", commaSep (map pp ts), ">" ]
  args = parens (commaSep (map ppSize (irtfSizeArgs call) ++ extraArgs))
  ppSize (x,s) = pp x <+> "as" <+> pp (sizeVarSizeTypeFor x s)

instance (PP tname, PP name) => PP (IRTopFunCall tname name) where
  pp = ppIRTopFunCall []

instance (PP tname, PP name, PP expr) => PP (IRCallable tname name expr) where
  pp call =
    case call of
      IRTopFun fu -> pp fu
      IRFunVal e  -> pp e

instance (PP tname, PP name, PP expr) => PP (IRCall tname name expr) where
  pp call = withPrec 0 $
            case ircFun call of
              IRTopFun fu -> ppIRTopFunCall args fu
              IRFunVal e  -> pp e <+> parens (commaSep args)

    where
    args = map pp (ircArgs call)

instance (PP tname, PP name, PP expr) => PP (IRExprF tname name expr) where
  pp expr =
    case expr of
      IRVar x -> pp x

      IRCallFun f -> pp f
      IRClosure f -> "[|" <+> pp f <+> "|]"

      IRIf e1 e2 e3  ->
        parensAfter 0 $
        withPrec 0 $
        vcat [ "if" <+> pp e1
             , nest 2 "then" <+> pp e2
             , nest 2 "else" <+> pp e3
             ]

      IRTuple es -> withPrec 0 (parens (commaSep (map pp es)))

      IRLet x e1 e2 ->
        withPrec 0 $
        vcat [ "let" <+> withTypes (pp x) <+> "=" <+> pp e1 <.> ";"
             , pp e2
             ]



instance (PP tname, PP name) => PP (IRExpr tname name) where
  pp (IRExpr e) = pp e

instance (PP tname, PP name) => PP (IRFunDecl tname name) where
  pp fd =
    vcat
      [ "fn" <+> pp (irfName fd) <+> tps <+> args <+> "->" <+> resT
      , nest 2 trts
      , "{"
      , nest 2 body
      , "}"
      ]
    where
    tps = case ftTypeParams ty of
            [] -> mempty
            ps -> hcat [ "<", commaSep (map pp ps), ">" ]

    trts = case ftTraits ty of
             [] -> mempty
             ts -> "where" <+> commaSep (map pp ts)


    args = parens
         $ withTypes
         $ commaSep
         $ map pp (ftSizeParams ty) ++ params

    ty = irfType fd

    (params,body) =
      case irfDef fd of
        IRFunDef is e   -> (map pp (zipWith IRName is (ftParams ty)), pp e)
        IRFunPrim       -> (map pp (ftParams ty), "/* primitive */")



    resT = withPrec 0 (pp (ftResult ty))


