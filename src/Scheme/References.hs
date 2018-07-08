{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Scheme.References where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Scheme.Types
import Scheme.References.Internal
import Scheme.CodeStruct (CodeStruct, CodeStructM)
import qualified Scheme.CodeStruct as Struct

import Types
import Operations


data ResolvedProgram = ResolvedProgram
                      { rAST :: RAST
                      , rpConstants :: [PyExpr]
                      , localVarNames :: [String]
                      } deriving (Show)
data RPState = RPState
             { currentConstants :: [PyExpr]
             , localTable :: Map String MutableRef
             } deriving (Show)

newtype ResolvedProgramM a =
  ResolvedProgramM (State RPState a)
  deriving (Functor, Applicative, Monad, MonadState RPState)

type ConstantRef = Reference 'ReadOnly
type MutableRef = Reference 'Writable

addConstant :: PyExpr -> ResolvedProgramM ConstantRef
addConstant expr = do
  l <- gets currentConstants
  let newConstantId = length l
  modify (\s -> s { currentConstants = l ++ [expr] })
  return (ConstantVarReference newConstantId)

lookupLocalVar :: String -> ResolvedProgramM (Maybe MutableRef)
lookupLocalVar varName = do
  table <- gets localTable
  return (Map.lookup varName table)

defineLocalVar :: String -> ResolvedProgramM MutableRef
defineLocalVar varName = do
  table <- gets localTable
  let newVarId = Map.size table
      ref = LocalVarReference newVarId
  
  modify (\s -> s { localTable = Map.insert varName ref table })

  return ref

getResolvedProgram :: ResolvedProgramM RAST -> ResolvedProgram
getResolvedProgram (ResolvedProgramM computation) =
  ResolvedProgram  { rAST = ast
                    , rpConstants = constants'
                    , localVarNames = Map.keys lookupTable
                    }
  where
    (ast, (RPState constants' lookupTable)) =
      runState computation initial

    initial = RPState [] Map.empty

-- compileToCodeStruct :: ResolvedProgram -> CodeStruct
-- compileToCodeStruct (ResolvedProgram ast constants') =
--   Struct.getCodeStruct compile
--   where
--     compile :: CodeStructM ()
--     compile = do
--       registerConstants
--       compileAST ast

--     registerConstants :: CodeStructM ()
--     registerConstants = forM_ constants' Struct.addConstant

--     compileAST :: ResolvedAST -> CodeStructM ()
--     compileAST (List (ASymbol "begin" : rest)) =
--       forM_ rest compileAST

--     compileAST (List [ASymbol "define", ASymbol var, expr]) = do
--       ref <- addLocalVar var
--       evaluateAndPutOnTop expr
--       storeTopOn ref

--     compileAST (List (ASymbol "print" : rest)) =
--       forM_  rest $ \expr -> do
--           evaluateAndPutOnTop expr
--           Struct.addInstr PRINT_EXPR

--     evaluateAndPutOnTop :: ResolvedAST -> CodeStructM ()
--     evaluateAndPutOnTop (ASymbol varName) = do
--       mRef <- getLocalVarRef varName
--       case mRef of
--         Nothing -> error ("Variable " ++ varName ++ " does not exist")
--         Just ref -> storeTopOn ref

--     evaluateAndPutOnTop (Atom ref) = putRefOnTop ref

--     putRefOnTop :: Reference a -> CodeStructM ()
--     putRefOnTop = Struct.addInstr . loadInstruction


--     storeTopOn :: Reference 'Writable -> CodeStructM ()
--     storeTopOn (LocalVarReference i) =
--       Struct.addInstr (STORE_FAST $ fromIntegral i)

-- loadInstruction :: Reference a -> Operation
-- loadInstruction (ConstantVarReference i) =
--   LOAD_CONST $ fromIntegral i

-- loadInstruction (LocalVarReference i) =
--   LOAD_FAST $ fromIntegral i

-- addLocalVar :: String -> CodeStructM (Reference 'Writable)
-- addLocalVar str = do
--   lVars <- gets Struct.localVars
--   let newVarId = Map.size lVars

--   modify (\s -> s { Struct.localVars = Map.insert str newVarId lVars }) 

--   return (LocalVarReference newVarId)

-- getLocalVarRef :: String -> CodeStructM (Maybe (Reference 'Writable))
-- getLocalVarRef varName = do
--   lVars <- gets Struct.localVars
--   return (LocalVarReference <$> Map.lookup varName lVars)

