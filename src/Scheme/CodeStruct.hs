{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.CodeStruct where

import Control.Monad.Except
import Control.Monad.State

import Scheme.Types
import Operations
import Types

data CodeStruct = CodeStruct
                    { localVars :: [String]
                    , instructions :: [Operation]
                    , consts :: [PyExpr]
                    } deriving (Show)

emptyCodeStruct :: CodeStruct
emptyCodeStruct = CodeStruct
  { localVars = []
  , instructions = []
  , consts = []
  }
  
newtype CodeStructM a = CodeStructM
  { unCodeStruct :: ExceptT CompileError (State CodeStruct) a
  }
  deriving (Functor,
            Applicative, Monad,
            MonadState CodeStruct,
            MonadError CompileError)

getCodeStruct :: CodeStructM () -> Either CompileError CodeStruct
getCodeStruct (CodeStructM computation) =
  eResult >> return codeStruct
  where
    (eResult, codeStruct) =
      runState (runExceptT computation) emptyCodeStruct 

addInstr :: Operation -> CodeStructM ()
addInstr op = do
  instrs <- gets instructions
  modify (\s -> s { instructions = instrs ++ [op] })

addConstant :: PyExpr -> CodeStructM ()
addConstant pyexpr = do
  consts' <- gets consts
  modify (\s -> s { consts = consts' ++ [pyexpr] })

addVariable :: String -> CodeStructM ()
addVariable varName = do
  vars <- gets localVars
  modify (\s -> s { localVars = vars ++ [varName] })
