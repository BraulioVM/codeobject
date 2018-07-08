module Scheme.Compilation where

resolveReferences :: AST -> ResolvedProgram
resolveReferences program =
  let
    (ast, constants') = runState (trackReferences program) []

  in ResolvedProgram { rAST = ast, rConstants = constants' }
  where
    trackReferences :: AST -> State [PyExpr] ResolvedAST
    trackReferences (Atom value) = do
      l <- get
      let ref = ConstantVarReference (length l)
      put (l ++ [toPyExpr value])

      return (Atom ref)
    
    trackReferences (ASymbol sym) = do
      return (ASymbol sym)

    trackReferences (List exprs) = do
      List <$> forM exprs trackReferences
