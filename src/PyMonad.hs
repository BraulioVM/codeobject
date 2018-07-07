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

data Computation = Instruction Operation [Computation]
                 | Load Reference

newtype RunValue = RunValue (PyMonad ())

loadReference :: Reference -> PyMonad ()
loadReference (LReference x) = addOp (LOAD_FAST x)
loadReference (CReference x) = addOp (LOAD_CONST x)

evaluate :: Computation -> PyMonad RunValue
evaluate (Instruction op computations) = return . RunValue $ do
  load computations
  addOp op

  where
    load :: [Computation] -> PyMonad ()
    load [] = return ()
    load (Load ref : xs) = loadReference ref >> load xs
    load ((Instruction op' comps) : xs) =
      load comps >> addOp op' >> load xs
evaluate (Load ref) = return . RunValue $ loadReference ref

sumop :: Computation -> Computation -> Computation
sumop c1 c2 = Instruction BINARY_ADD [c1, c2]

makeComp :: Reference -> Computation
makeComp = Load

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

printComp :: Computation -> PyMonad ()
printComp = evaluate >=> printValue

printValue :: RunValue -> PyMonad ()
printValue (RunValue value) = do
  value
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


