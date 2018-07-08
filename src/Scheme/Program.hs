{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.Program where

import Control.Monad.State

import Scheme.Types
import Scheme.References
import Operations

compile :: ResolvedAST -> CodeGen ()
compile (List (ASymbol "begin" : rest)) =
  forM_ rest compile

compile (List [ASymbol "define", ASymbol var, expr]) = do
  ref <- addLocalVar var
  evaluateAndPutOnTop expr
  storeTopOn ref

compile (List (ASymbol "print" : rest)) =
  forM_  rest $ \expr -> do
      evaluateAndPutOnTop expr
      addInstr PRINT_EXPR

evaluateAndPutOnTop :: ResolvedAST -> CodeGen ()
evaluateAndPutOnTop (ASymbol varName) = do
  mRef <- getLocalVarRef varName
  case mRef of
    Nothing -> error ("Variable " ++ varName ++ " does not exist")
    Just ref -> storeTopOn ref

evaluateAndPutOnTop (Atom ref) = putRefOnTop ref

genCode :: AST -> CodeGenState
genCode program =
  execState
  (unCodeGen . compile . getAST $ resolvedAST)
  initialCodeGenState
  where
    resolvedAST = resolveReferences program
    initialCodeGenState = getInitialState resolvedAST

testProgram :: AST
testProgram = 
  List [ ASymbol "begin"
       , List [ ASymbol "define"
              , ASymbol "x"
              , Atom (AInt 10)
              ]
       , List [ ASymbol "print"
              , Atom (AString "hey")
              , ASymbol "x"
              ]
       ]
                
