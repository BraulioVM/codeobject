module Scheme.References ( Reference
                         , ResolvedAST
                         , resolveReferences
                         , getInitialState
                         , storeTopOn
                         , addLocalVar
                         , getLocalVarRef
                         , getAST
                         , putRefOnTop) where

import Control.Monad (forM)
import Control.Monad.State
import qualified Data.Map as Map
import Scheme.Types

import Types
import Operations

data Reference = LocalVarReference Int
               | ConstantVarReference Int

type ResolvedAST = AbstractProgram Reference

data ResolvedProgram = ResolvedProgram
                       { rAST :: ResolvedAST
                       , rConstants :: [PyExpr]
                       }

getAST :: ResolvedProgram -> ResolvedAST
getAST = rAST

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

getInitialState :: ResolvedProgram -> CodeGenState
getInitialState (ResolvedProgram _ rconsts) =
  CodeGenState { localVars = Map.empty
               , instructions = []
               , consts = rconsts
               }

storeTopOn :: Reference -> CodeGen ()
storeTopOn (LocalVarReference i) =
  addInstr (STORE_FAST $ fromIntegral i)

putRefOnTop :: Reference -> CodeGen ()
putRefOnTop (ConstantVarReference i) =
  addInstr (LOAD_CONST $ fromIntegral i)

putRefOnTop (LocalVarReference i) =
  addInstr (LOAD_FAST $ fromIntegral i)

addLocalVar :: String -> CodeGen Reference
addLocalVar str = do
  lVars <- gets localVars
  let newVarId = Map.size lVars

  modify (\s -> s { localVars = Map.insert str newVarId lVars }) 

  return (LocalVarReference newVarId)

getLocalVarRef :: String -> CodeGen (Maybe Reference)
getLocalVarRef varName = do
  lVars <- gets localVars
  return (LocalVarReference <$> Map.lookup varName lVars)
