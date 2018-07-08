{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.CodeStruct where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map


import Operations
import Types

data CodeStruct = CodeStruct
                    { localVars :: Map String Int
                    , instructions :: [Operation]
                    , consts :: [PyExpr]
                    } deriving (Show)

emptyCodeStruct :: CodeStruct
emptyCodeStruct = CodeStruct
  { localVars = Map.empty
  , instructions = []
  , consts = []
  }
  
newtype CodeStructM a = CodeStructM
  { unCodeStruct :: State CodeStruct a
  }
  deriving (Functor, Applicative, Monad, MonadState CodeStruct)

getCodeStruct :: CodeStructM () -> CodeStruct
getCodeStruct (CodeStructM computation) =
  execState computation emptyCodeStruct 

addInstr :: Operation -> CodeStructM ()
addInstr op = do
  instrs <- gets instructions
  modify (\s -> s { instructions = instrs ++ [op] })

addConstant :: PyExpr -> CodeStructM ()
addConstant pyexpr = do
  consts' <- gets consts
  modify (\s -> s { consts = consts' ++ [pyexpr] })
