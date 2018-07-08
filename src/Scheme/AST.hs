module Scheme.AST where

import Control.Monad (forM)

import Types
import Scheme.Types
import Scheme.References

resolveReferences :: FAST -> Either CompileError ResolvedProgram
resolveReferences program =
  Right $ getResolvedProgram (trackReferences program)
  where
    trackReferences :: FAST -> ResolvedProgramM RAST
    trackReferences (FAtom x) = do
      ref <- addConstant (toPyExpr x)
      return (FAtom ref)
      
    trackReferences (FReference str) = do
      mRef <- lookupLocalVar str
      case mRef of
        Nothing -> error "undefined local var"
        Just ref -> return (FReference ref)

    trackReferences (FBegin forms) = do
      FBegin <$> forM forms trackReferences

    trackReferences (FDefine varName expr) = do
      -- don't allow circular references
      resolvedExpr <- trackReferences expr
      ref <- defineLocalVar varName
      return (FDefine ref resolvedExpr)

    trackReferences (FApply funcName parameters) = do
      mFuncRef <- lookupLocalVar funcName
      case mFuncRef of
        Nothing -> error "function not defined"
        Just funcRef ->
          FApply funcRef <$> forM parameters trackReferences
