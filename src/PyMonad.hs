{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PyMonad where

import Control.Monad.State
import Data.Word

import Operations
import Types

data CodeObjectState = CodeObjectState
                       { localVariables :: [String]
                       , operations :: [Operation]
                       , globalVariables :: [String]
                       , constantVariables :: [PyExpr]
                       } deriving (Show)
    
initialCodeObjectState :: CodeObjectState
initialCodeObjectState = CodeObjectState [] [] [] [PyNone]

newtype PyMonad a = PyMonad (State CodeObjectState a)
  deriving (Functor, Applicative, Monad, MonadState CodeObjectState)

data Reference = LReference Word16
               | CReference Word16

defLocalVariable :: String -> PyMonad Reference
defLocalVariable varName = do
  modify (\s -> s { localVariables = localVariables s ++ [varName] })
  s <- get
  return (LReference $ fromIntegral $ length (localVariables s))

defConstant :: PyExpr -> PyMonad Reference
defConstant expr = do
  modify $ \s ->
    s { constantVariables = constantVariables s ++ [expr] }

  s <- get
  return $ CReference (fromIntegral $ (length $ constantVariables s) - 1)

addOp :: Operation -> PyMonad ()
addOp op = modify $ \s ->
  s { operations = operations s ++ [op] }

printRef :: Reference -> PyMonad ()
printRef ref = do
  case ref of
    LReference i -> addOp $ LOAD_FAST i
    CReference i -> addOp $ LOAD_CONST i
  addOp PRINT_EXPR

createCodeObject :: PyMonad () -> CodeObject
createCodeObject (PyMonad m) =
  defaultObject { nLocals = length lVars
                , codeString = getByteCode $ ops ++
                               [LOAD_CONST 0
                               , RETURN_VALUE]
                , names = PTuple gVars
                , varNames = PTuple lVars
                , constants = PTuple consts
                }
  where
    (CodeObjectState lVars ops gVars consts) =
      execState m initialCodeObjectState


