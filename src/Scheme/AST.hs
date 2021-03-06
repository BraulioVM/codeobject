{-# LANGUAGE GADTs #-}
module Scheme.AST where

import Control.Monad (forM)

import Scheme.Types
import Scheme.References

resolveReferences :: FAST -> Either CompileError Scope
resolveReferences program =
  getResolvedProgram (trackReferences program)
  where
    trackReferences :: FAST -> ScopeM NamedAST
    trackReferences (FAtom x) = do
      constRef <- addBasicConstant x
      return (FAtom constRef)
      
    trackReferences (FReference str) = do
      _ <- requireVar str
      return (FReference str)

    trackReferences (FBegin forms) = do
      FBegin <$> forM forms trackReferences

    trackReferences (FDefine varName expr) = do
      -- don't allow circular references
      resolvedExpr <- trackReferences expr
      localRegisterVar varName LocalScope
      return (FDefine varName resolvedExpr)

    trackReferences (FApply funcExpr parameters) = do
      FApply <$> trackReferences funcExpr <*>
        forM parameters trackReferences
      
    trackReferences (FLambda params code) = do
      (ident, freeVars) <- defineSubScope params (trackReferences code) 
      return (FFuncRef ident freeVars)
