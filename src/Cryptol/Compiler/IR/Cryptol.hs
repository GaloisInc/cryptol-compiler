-- | Specialization for Cryptol names
module Cryptol.Compiler.IR.Cryptol
  ( module M
  , module Cryptol.Compiler.IR.Cryptol
  ) where

import Cryptol.TypeCheck.AST qualified as Cry

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
type Name = IRName Cry.TParam Cry.Name

-- | NameIds, specialized to Cryptol names
type NameId = IRNameId Cry.Name

-- | Function names, specialized to Cryptol names
type FunName = IRFunName Cry.Name

-- | Various types of names, specialize to Cryptol names
type FunNameFlavor = IRFunNameFlavor Cry.Name

-- | Declarations, specialized to Cryptol names
type FunDecl = IRFunDecl Cry.TParam Cry.Name

-- | Function definitions, specialized to Cryptol names
type FunDef = IRFunDef Cry.TParam Cry.Name

-- | Expressions, specialized to Cryptol names
type Expr = IRExpr Cry.TParam Cry.Name


