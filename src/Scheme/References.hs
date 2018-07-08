{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.References ( Reference
                         , ResolvedAST
                         , ResolvedProgramM
                         , ResolvedProgram
                         , addConstant
                         , compileToCodeStruct
                         , getResolvedProgram
                         , addLocalVar
                         , getLocalVarRef
                         ) where

import Control.Monad.State
import qualified Data.Map as Map

import Scheme.Types
import Scheme.References.Internal
import Scheme.CodeStruct (CodeStruct, CodeStructM)
import qualified Scheme.CodeStruct as Struct

import Types
import Operations


data ResolvedProgram = ResolvedProgram
                       { rAST :: ResolvedAST
                       , rConstants :: [PyExpr]
                       }

newtype ResolvedProgramM a =
  ResolvedProgramM (State [PyExpr] a)
  deriving (Functor, Applicative, Monad, MonadState [PyExpr])

addConstant :: PyExpr -> ResolvedProgramM Reference
addConstant expr = do
  currentConstantsLength <- gets length
  modify (++[expr])
  
  return (ConstantVarReference $ currentConstantsLength)

getResolvedProgram :: ResolvedProgramM ResolvedAST -> ResolvedProgram
getResolvedProgram (ResolvedProgramM computation) =
  let
    (ast, constants') = runState computation []
  in ResolvedProgram  { rAST = ast, rConstants = constants' }


compileToCodeStruct :: ResolvedProgram -> CodeStruct
compileToCodeStruct (ResolvedProgram ast constants') =
  Struct.getCodeStruct compile
  where
    compile :: CodeStructM ()
    compile = do
      registerConstants
      compileAST ast

    registerConstants :: CodeStructM ()
    registerConstants = forM_ constants' Struct.addConstant

    compileAST :: ResolvedAST -> CodeStructM ()
    compileAST (List (ASymbol "begin" : rest)) =
      forM_ rest compileAST

    compileAST (List [ASymbol "define", ASymbol var, expr]) = do
      ref <- addLocalVar var
      evaluateAndPutOnTop expr
      storeTopOn ref

    compileAST (List (ASymbol "print" : rest)) =
      forM_  rest $ \expr -> do
          evaluateAndPutOnTop expr
          Struct.addInstr PRINT_EXPR

    evaluateAndPutOnTop :: ResolvedAST -> CodeStructM ()
    evaluateAndPutOnTop (ASymbol varName) = do
      mRef <- getLocalVarRef varName
      case mRef of
        Nothing -> error ("Variable " ++ varName ++ " does not exist")
        Just ref -> storeTopOn ref

    evaluateAndPutOnTop (Atom ref) = putRefOnTop ref

    putRefOnTop :: Reference -> CodeStructM ()
    putRefOnTop = Struct.addInstr . loadInstruction


    storeTopOn :: Reference -> CodeStructM ()
    storeTopOn (LocalVarReference i) =
      Struct.addInstr (STORE_FAST $ fromIntegral i)

loadInstruction :: Reference -> Operation
loadInstruction (ConstantVarReference i) =
  LOAD_CONST $ fromIntegral i

loadInstruction (LocalVarReference i) =
  LOAD_FAST $ fromIntegral i

addLocalVar :: String -> CodeStructM Reference
addLocalVar str = do
  lVars <- gets Struct.localVars
  let newVarId = Map.size lVars

  modify (\s -> s { Struct.localVars = Map.insert str newVarId lVars }) 

  return (LocalVarReference newVarId)

getLocalVarRef :: String -> CodeStructM (Maybe Reference)
getLocalVarRef varName = do
  lVars <- gets Struct.localVars
  return (LocalVarReference <$> Map.lookup varName lVars)

