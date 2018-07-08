module Scheme.AST where

import Control.Monad (forM)

import Types
import Scheme.Types
import Scheme.References

resolveReferences :: AST -> ResolvedProgram
resolveReferences program =
  getResolvedProgram (trackReferences program)
  where
    trackReferences :: AST -> ResolvedProgramM ResolvedAST
    trackReferences (Atom x) = do
      ref <- addConstant (toPyExpr x)
      return (Atom ref)
  
    trackReferences (ASymbol str) =
      return (ASymbol str)

    trackReferences (List exprs) =
      List <$> forM exprs trackReferences
