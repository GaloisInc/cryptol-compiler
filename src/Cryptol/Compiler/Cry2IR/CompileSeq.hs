module Cryptol.Compiler.Cry2IR.CompileSeq where

import qualified Data.Text as Text
import Cryptol.TypeCheck.AST qualified as Cry
import Cryptol.TypeCheck.Solver.InfNat qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Cry2IR.RecursiveStreams
import Cryptol.Compiler.Cry2IR.Monad

type Seq = IRSeq Cry.TParam NameId Expr


compileRecSeqs :: [(Name, Seq)] -> SpecM Expr -> SpecM Expr
compileRecSeqs ds _k = unsupported (dbg ds)
  where
  dbg = Text.pack . show . vcat . map ppDef
  ppDef (x,d) = pp x <+> "=" <+> pp d

