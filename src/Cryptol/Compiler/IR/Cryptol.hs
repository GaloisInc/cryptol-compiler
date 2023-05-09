-- | Specialization for Cryptol names
module Cryptol.Compiler.IR.Cryptol
  ( module M
  , module Cryptol.Compiler.IR.Cryptol
  ) where

import Cryptol.TypeCheck.AST qualified as Cry

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


data NameId =
    AnonId !Int
  | NameId Cry.Name
    deriving (Eq,Ord)

instance PP NameId where
  pp name =
    case name of
      AnonId n -> "IR::x_" <.> pp n
      NameId n -> pp n






