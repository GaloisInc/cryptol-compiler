-- | Specialization for Cryptol names
module Cryptol.Compiler.IR.Cryptol
  ( module M
  , module Cryptol.Compiler.IR.Cryptol
  ) where

import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.Utils.Ident qualified as Cry

import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Type as M
import Cryptol.Compiler.IR as M

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

-- | Size types, specialized to Cryptol names
type SizeName = IRSizeName Cry.TParam

-- | Names, specialized to Cryptol names
type Name = IRName Cry.TParam NameId

-- | Function names, specialized to Cryptol names
type FunName = IRFunName NameId

-- | Various types of names, specialize to Cryptol names
type FunNameFlavor = IRFunNameFlavor NameId

-- | Declarations, specialized to Cryptol names
type FunDecl = IRFunDecl Cry.TParam NameId

-- | Function definitions, specialized to Cryptol names
type FunDef = IRFunDef Cry.TParam NameId

-- | Expressions, specialized to Cryptol names
type Expr = IRExpr Cry.TParam NameId

-- | Stream expressions, specialized to Cryptol types
type StreamExpr = IRStreamExpr Cry.TParam NameId Expr

-- | Function calls, specialized to Cryptol names
type Call = IRCall Cry.TParam NameId Expr


data NameId =
    AnonId !Int
  | NameId Cry.Name
    deriving (Eq,Ord)

-- | Get the Crytpol module for a top-level `NameId`.
nameIdModule :: NameId -> Cry.ModName
nameIdModule f =
  Cry.nameTopModule
  case f of
    NameId x -> x
    AnonId {} -> panic "nameIdModule" [ "Unexpected anonymous function name" ]

instance PP NameId where
  pp name =
    case name of
      AnonId n -> "IR::x_" <.> pp n
      NameId n -> pp n






