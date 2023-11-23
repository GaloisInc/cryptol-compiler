-- | Basic IR for code generation.
module Cryptol.Compiler.IR
  ( -- * Functions
    IRFunDecl(..)
  , IRFunDef(..)
  , IRFunName(..)
  , IRFunNameFlavor(..)
  , IRPrim(..)
  , FunInstance(..)
  , ParamInfo(..)

    -- * Expressions
  , IRExpr(..)
  , IRExprF(..)

    -- ** Local Names
  , IRName(..)

    -- ** Function Calls
  , IRCall(..)
  , IRCallable(..)
  , IRTopFunCall(..)
  , topCallTypeArgs
  , callPrimG
  , callSizePrim
  , callPrim
  , mkTuple

    -- ** Streams
  , IRStreamExpr(..)

    -- * Types
  , IRType(..)
  , HasType(..)

    -- ** Size "Types"
  , IRStreamSize(..)
  , IRSize(..)
  , IRSizeName(..)
  , SizeVarSize(..)
  , isKnownStreamSize
  , isKnownSize
  , pattern K
  , maxSizeVal

    -- ** Function Types
  , IRFunType(..)

    -- ** Traits
  , IRTrait(..)
  , IRTraitName
  , traitMap

    -- * Type functions
  , VName
  , TName

  ) where

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Common as Exported
import Cryptol.Compiler.IR.Type   as Exported
import Cryptol.Compiler.IR.Prims

--------------------------------------------------------------------------------

-- | Function name, paired with an instance.
data IRFunName name = IRFunName
  { irfnName     :: IRFunNameFlavor name
  , irfnInstance :: FunInstance
  } deriving (Eq,Ord,Functor,Foldable,Traversable)

-- | Various types of function names
data IRFunNameFlavor name =
    IRPrimName IRPrim                 -- ^ An IR primtive
  | IRDeclaredFunName name            -- ^ A declared function
    deriving (Eq,Ord,Functor,Foldable,Traversable)

-- | Typed names.  When compared, we only consider the name, not the type.
data IRName tname name =
  IRName
    { irNameName :: name
    , irNameType :: IRType tname
    }

instance Eq name => Eq (IRName tname name) where
  IRName x _ == IRName y _ = x == y

instance Ord name => Ord (IRName tname name) where
  compare (IRName x _) (IRName y _) = compare x y

-- | A function declaration.
data IRFunDecl tname name =
  IRFunDecl
    { irfName       :: IRFunName name
    , irfType       :: IRFunType tname
    , irfDef        :: IRFunDef tname name
    }

-- | The definition of a top-level function.
data IRFunDef tname name =
    IRFunPrim                             -- ^ A primitive function.
  | IRFunDef [name] (IRExpr tname name)
    -- ^ Function with definition.
    -- Includes names of the function parameters.
    -- Their types are in the function's type.

-- | Expressions
newtype IRExpr tname name = IRExpr (IRExprF tname name (IRExpr tname name))

-- | The various flavours of expressions.
-- Expressions are split in two parts because this makes it easier to
-- write generic traversals.
data IRExprF tname name expr =
    IRVar     (IRName tname name)
  | IRCallFun (IRCall tname name expr)    -- ^ Call a function
  | IRClosure (IRCall tname name expr)    -- ^ Partial application
  | IRLam [IRName tname name] expr        -- ^ Lambda
  | IRIf expr expr expr
  | IRLet (IRName tname name) expr expr
  | IRStream (IRStreamExpr tname name expr)
    deriving (Functor,Foldable,Traversable)


-- | Something that can be called.
data IRCallable tname name expr =
    IRTopFun (IRTopFunCall tname name)    -- ^ A top level declaration
  | IRFunVal expr                         -- ^ A closure value
    deriving (Functor,Foldable,Traversable)

-- | A call to a top level function, containing common parameters
-- that are always present.
data IRTopFunCall tname name =
  IRTopFunCall
    { irtfName      :: IRFunName name
    , irtfTypeArgs  :: [IRType tname]               -- ^ Type arguments
    , irtfSizeArgs  :: [(IRSize tname,SizeVarSize)] -- ^ Size arguments
    }

-- | Information about a function call or a closure.
-- Note: we keep the types of arguments around, as they are convenient
-- to have for backends that use more refined representations of types,
-- and so the argument types need to be adjusted.
data IRCall tname name expr =
  IRCall
    { ircFun      :: IRCallable tname name expr  -- ^ What we are calling

    , ircFunType  :: !(IRFunType tname)
      -- ^ Type of the function we are calling (uninstantiated)

    , ircArgTypes :: [IRType tname]   -- ^ Types of arguments (types of ircArgs)
    , ircResType  :: IRType tname     -- ^ Result of function, or closure type
    , ircArgs     :: [expr]           -- ^ Available arguments
    } deriving (Functor,Foldable,Traversable)


data IRStreamExpr tname name expr =
  IRStreamExpr
    { irsType    :: IRType tname      -- ^ TStream l a
    , irsExterns :: [(IRName tname name, expr)]
    , irsInit    :: expr              -- ^ TArray hist a
    , irsNext    :: expr              -- ^ a
    } deriving (Functor,Foldable,Traversable)

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
type instance TName (IRStreamExpr tname name expr) = tname

type family VName a
type instance VName [a]       = VName a
type instance VName (Maybe a) = VName a
type instance VName (a,b)     = VName a

type instance VName (IRName tname name) = name
type instance VName (IRFunDecl tname name) = name
type instance VName (IRFunDef tname name) = name
type instance VName (IRExpr tname name) = name
type instance VName (IRExprF tname name expr) = name
type instance VName (IRCall tname name expr) = name
type instance VName (IRTopFunCall tname name) = name
type instance VName (IRCallable tname name expr) = name
type instance VName (IRStreamExpr tname name expr) = name



-- | Things that have a type.
class HasType t where
  -- | Compute the type of something.
  typeOf :: t -> IRType (TName t)

instance HasType (IRName tname name) where
  typeOf (IRName _ t) = t

instance (HasType expr, TName expr ~ tname) =>
  HasType (IRExprF tname name expr) where
  typeOf expr =
    case expr  of
      IRVar x           -> typeOf x
      IRCallFun f       -> typeOf f
      IRClosure f       -> typeOf f
      IRLam xs e        -> TFun (map typeOf xs) (typeOf e)
      IRIf _ x _        -> typeOf x
      IRLet _ _ e       -> typeOf e
      IRStream e        -> typeOf e

instance (HasType expr, TName expr ~ tname) =>
  HasType (IRStreamExpr tname name expr) where
  typeOf = irsType

instance HasType (IRCall tname name expr) where
  typeOf = ircResType

instance HasType (IRExpr tname name) where
  typeOf (IRExpr e) = typeOf e


--------------------------------------------------------------------------------
-- Common Utilities

-- | Call a primitive (no instances, or type arguments).
callPrimG ::
  IRPrim                        {- ^ Primitive name -} ->
  [(IRSize tname,SizeVarSize)]  {-^ Size arguments -} ->
  [IRExpr tname name]           {-^ Expression arguments -} ->
  IRType tname                  {-^ Result type -} ->
  IRExpr tname name
callPrimG prim szs es tgtT =
  IRExpr $ IRCallFun
    IRCall
      { ircFun =
          IRTopFun
            IRTopFunCall
               { irtfName =
                   IRFunName
                    { irfnName     = IRPrimName prim
                    , irfnInstance = FunInstance []
                    }
               , irtfTypeArgs = []
               , irtfSizeArgs = szs
               }
      , ircFunType = monoFunType (map typeOf es) tgtT
      , ircArgTypes = map typeOf es
      , ircResType = tgtT
      , ircArgs = es
      }




callSizePrim ::
  IRPrim ->
  [(IRSize tname, SizeVarSize)] ->
  IRType tname ->
  IRExpr tname name
callSizePrim prim es = callPrimG prim es []

callPrim :: IRPrim -> [IRExpr tname name] -> IRType tname -> IRExpr tname name
callPrim prim = callPrimG prim []

mkTuple :: [IRExpr tname name] -> IRExpr tname name
mkTuple es = callPrim Tuple es (TTuple (map typeOf es))

topCallTypeArgs :: IRTopFunCall tname name -> [IRType tname]
topCallTypeArgs tf = go ps0 (irtfTypeArgs tf)
  where
  FunInstance ps0 = irfnInstance (irtfName tf)
  go ps ts =
    case ps of
      [] ->
        case ts of
          [] -> []
          _  -> panic "instanceTypeArgs" ["Extra type arguments"]
      p : more ->
        case p of
          NumFixed {} -> go more ts
          NumVar {}   -> go more ts
          TyBool      -> TBool : go more ts
          TyNotBool ->
            case ts of
              t : moreTs -> t : go more moreTs
              [] -> panic "instanceTypeArgs" ["Missing type arg"]
          TyAny ->
            case ts of
              t : moreTs -> t : go more moreTs
              [] -> panic "instanceTypeArgs" ["Missing type arg"]



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
  ppSize (x,s) = pp x <+> "as" <+> pp s

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

      IRLet x e1 e2 ->
        withPrec 0 $
        vcat [ "let" <+> withTypes (pp x) <+> "=" <+> pp e1 <.> ";"
             , pp e2
             ]

      IRLam xs e ->
        parensAfter 0 $
        withPrec 0 $
        hang (withTypes (hcat [ "|", commaSep (map pp xs), "|" ])) 2 (pp e)

      IRStream e -> pp e


instance (PP tname, PP name, PP expr) =>
  PP (IRStreamExpr tname name expr) where
  pp expr =
    vcat [ "stream" <+> "{"
         , nest 2 (pp (irsInit expr))
         , "#"
         , nest 2 (pp (irsNext expr))
         , ppExterns
         ]
    where
    ppExtern (x,e) = pp x <+> "=" <+> pp e
    ppExterns = case irsExterns expr of
                  [] -> mempty
                  ds -> "where" $$ nest 2 (vcat (map ppExtern ds))

instance (PP tname, PP name) => PP (IRExpr tname name) where
  pp (IRExpr e) = pp e

instance (PP tname, PP name) => PP (IRFunDecl tname name) where
  pp fd =
    vcat
      [ hang ("fn" <+> pp (irfName fd) <+> tps) 2
        (hang args 2 ("->" <+> resT))
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


