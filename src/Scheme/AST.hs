module Scheme.AST where


import Control.Monad (forM)
import Control.Monad.Except

import Types
import Scheme.Types
import Scheme.References

resolveReferences :: FAST -> Either CompileError ResolvedProgram
resolveReferences program =
  getResolvedProgram (trackReferences program)
  where
    trackReferences :: FAST -> ResolvedProgramM NRAST
    trackReferences (FAtom x) = do
      ref <- addConstExprToScope (toPyExpr x)
      return (FAtom ref)
      
    trackReferences (FReference str) = do
      mRef <- lookupVar str
      case mRef of
        Nothing -> throwError (UndefinedVariable str)
        Just ref -> return (FReference ref)

    trackReferences (FBegin forms) = do
      FBegin <$> forM forms trackReferences

    trackReferences (FDefine varName expr) = do
      -- don't allow circular references
      resolvedExpr <- trackReferences expr
      ref <- defineLocalVar varName
      return (FDefine ref resolvedExpr)

    trackReferences (FApply funcName parameters) = do
      mFuncRef <- lookupVar funcName
      case mFuncRef of
        Nothing -> throwError (UndefinedVariable funcName)
        Just funcRef ->
          FApply funcRef <$> forM parameters trackReferences

    trackReferences (FLambda params code) = do
      ref <- withNewScope params (trackReferences code) 
      return (FAtom ref)
