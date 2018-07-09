{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Scheme.References where

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Exts


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
  ResolvedProgramM (ExceptT CompileError (State RPState) a)
  deriving (Functor
           , Applicative
           , Monad
           , MonadState RPState
           , MonadError CompileError)

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

getResolvedProgram :: ResolvedProgramM RAST
                   -> Either CompileError ResolvedProgram
getResolvedProgram (ResolvedProgramM computation) = do
  ast <- eAST
  let (RPState constants' lookupTable) = rpState
  return $ ResolvedProgram  { rAST = ast
                            , rpConstants = constants'
                            , localVarNames = (getLocalVarNames lookupTable)
                            }
  where
    (eAST, rpState) =
      runState (runExceptT computation) initial

    initial = RPState [] Map.empty

    getLocalVarNames :: Map String MutableRef -> [String]
    getLocalVarNames table =
      fst <$> sortedItems
      where
        sortedItems = sortWith snd (Map.toList table) 
    

compileToCodeStruct :: ResolvedProgram -> Either CompileError CodeStruct
compileToCodeStruct (ResolvedProgram ast constants' localVars) = do
  Struct.getCodeStruct compile

  where
    compile :: CodeStructM ()
    compile = do
      registerConstants
      registerLocalVariables
      compileAST ast

    registerConstants :: CodeStructM ()
    registerConstants = forM_ constants' Struct.addConstant

    registerLocalVariables :: CodeStructM ()
    registerLocalVariables = forM_ localVars Struct.addVariable

    compileAST :: RAST -> CodeStructM ()
    compileAST (FAtom _) = return ()
    compileAST (FReference _) = return ()

    compileAST (FBegin rest) = do
      forM_ rest compileAST

    compileAST (FDefine ref expr) = do
      evaluateAndPutOnTop expr
      storeTopOn ref

    compileAST _ = throwError (NotImplemented "function calls")

    -- compileAST (FApply (ASymbol "print" : rest)) =
    --   forM_  rest $ \expr -> do
    --       evaluateAndPutOnTop expr
    --       Struct.addInstr PRINT_EXPR

    evaluateAndPutOnTop :: RAST -> CodeStructM ()
    evaluateAndPutOnTop (FAtom ref) = putRefOnTop ref
    evaluateAndPutOnTop (FReference ref) = putRefOnTop ref
    evaluateAndPutOnTop (FBegin rest) = do
      putLastOnTop rest
      where
        putLastOnTop :: [RAST] -> CodeStructM ()
        putLastOnTop [] = return ()
        putLastOnTop [x] = evaluateAndPutOnTop x
        putLastOnTop (x:xs) = compileAST x >> putLastOnTop xs
    
    evaluateAndPutOnTop a@(FDefine ref _) = do
      compileAST a
      putRefOnTop ref

    evaluateAndPutOnTop _ = throwError (NotImplemented "calls")

    putRefOnTop :: Reference a -> CodeStructM ()
    putRefOnTop = Struct.addInstr . loadInstruction

    storeTopOn :: Reference 'Writable -> CodeStructM ()
    storeTopOn (LocalVarReference i) =
      Struct.addInstr (STORE_FAST $ fromIntegral i)

loadInstruction :: Reference a -> Operation
loadInstruction (ConstantVarReference i) =
  LOAD_CONST $ fromIntegral i

loadInstruction (LocalVarReference i) =
  LOAD_FAST $ fromIntegral i
