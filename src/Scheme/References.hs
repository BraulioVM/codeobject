{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Scheme.References where

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Scheme.Types
import Scheme.References.Types
import Scheme.References.Internal

import Types hiding (name)

newtype ResolvedProgram = ResolvedProgram
  { rpGlobalScope :: Scope
  }
  deriving (Show, Eq)
                      
data Scope = Scope
             { scopeAST :: NRAST
             , scopeConstants :: [Either PyExpr Scope]
             , scopeLocalVariables :: Map String MutableRef
             , scopeCellVariables :: Map String MutableRef
             , scopeFreeVariables :: Map String MutableRef
             } deriving (Show, Eq)

type ConstantRef = Reference 'ReadOnly
type MutableRef = NReference 'Writable

type LookupTable = Map String MutableRef

data Function = Function { fAST :: NRAST
                         , fScopeState :: ScopeState
                         } deriving (Show, Eq)

data ScopeState = ScopeState
                  { ssConstants :: [Either PyExpr Function]
                  , ssLocalTable :: LookupTable
                  } deriving (Show, Eq)

nOfLocalVars :: ScopeState -> Int
nOfLocalVars (ScopeState _ localTable) =
  length (filter isLocal $ Map.elems localTable)
  
data ScopePtr = Top ScopeState -- ^ A top level RPState pointer
              | Inner ScopeState ScopePtr -- ^ An inner pointer
                deriving (Show, Eq)

innerScope :: ScopePtr -> ScopeState
innerScope (Top ss) = ss
innerScope (Inner ss _) = ss

nOfLocalVarsInInnerScope :: ScopePtr -> Int
nOfLocalVarsInInnerScope = nOfLocalVars . innerScope

updateInnerState :: (ScopeState -> ScopeState)
                 -> (ScopePtr -> ScopePtr)
updateInnerState f (Top rs) = Top (f rs)
updateInnerState f (Inner rs parent) = Inner (f rs) parent

newtype ResolvedProgramM a =
  ResolvedProgramM (ExceptT CompileError (State ScopePtr) a)
  deriving (Functor
           , Applicative
           , Monad
           , MonadState ScopePtr
           , MonadError CompileError)

addConstExprToScope :: PyExpr -> ResolvedProgramM ConstantRef
addConstExprToScope = addConstantToScope . Left

addConstantToScope :: Either PyExpr Function
                   -> ResolvedProgramM ConstantRef
addConstantToScope expr = do
  newConstantId <- gets innerNewConstantId
  modify (updateInnerState pAddConstant)

  return (ConstantVarReference newConstantId)
  where
    pAddConstant ss =
      ss { ssConstants = (ssConstants ss) ++ [expr] }
    
    innerNewConstantId :: ScopePtr -> Int
    innerNewConstantId =
      length . ssConstants . innerScope

lookupVar :: String -> ResolvedProgramM (Maybe MutableRef)
lookupVar varName = do
  currentPtr <- get
  mResult <- recursiveLookup varName currentPtr True

  case mResult of
    Nothing -> return Nothing
    Just (newPtr, ref) -> do
      let localTable = ssLocalTable (innerScope newPtr)
          newLocalTable = Map.insert varName ref localTable
          newPtr' = updateInnerState (\ss -> ss { ssLocalTable = newLocalTable }) newPtr

      put newPtr'
      return (Just ref)

  where
    localLookup :: String
                -> ScopeState
                -> Maybe MutableRef
    localLookup name ss = do
      ref <- Map.lookup name (ssLocalTable ss)
      return ref

    recursiveLookup :: String
                    -> ScopePtr
                    -> Bool
                    -> ResolvedProgramM (Maybe (ScopePtr, MutableRef))
    recursiveLookup name ptr@(Top ss) True =
      return $ (ptr,) <$> localLookup name ss

    recursiveLookup name (Top ss) False = do
      let mRef = localLookup name ss
      case mRef of
        Nothing -> return Nothing
        Just _ -> do
          let currentTable = ssLocalTable ss
              newSS = ss
                { ssLocalTable =
                    Map.insert name (CellVarReference name) currentTable }
          return $ Just (Top newSS, FreeVarReference name)
      
    recursiveLookup name ptr@(Inner ss parent) True = do
      case localLookup name ss of
        Nothing -> do
          mResult <- recursiveLookup name parent False
          case mResult of
            Nothing -> return Nothing
            Just (newParent, ref) ->
              return $ Just (Inner ss newParent, ref)
        Just ref -> return $ Just (ptr, ref)
     
    recursiveLookup name (Inner ss parent) False = do
      case localLookup name ss of
        Nothing -> do
          mResult <- recursiveLookup name parent False
          case mResult of
            Nothing -> return Nothing
            Just (newParent, ref) ->
              return $ Just (Inner ss newParent, ref)
        Just _ -> do -- we have to make it a cell var
          let table = ssLocalTable ss
              newTable =
                Map.insert name (CellVarReference name) table
              newSS = ss { ssLocalTable = newTable }
              newPtr = Inner newSS parent
          return $ Just (newPtr, FreeVarReference name)

withNewScope :: [ref]
             -> ResolvedProgramM NRAST
             -> ResolvedProgramM ConstantRef
withNewScope _ act = do
  modify (\scopePtr -> Inner emptySS scopePtr)
  rast <- act
  scopePtr <- get
    
  case scopePtr of
    Top _ -> throwError ScopeError
    Inner ss parent -> do
      let function = Function rast ss
      put parent
      addConstantToScope (Right function)

  where
    emptySS = ScopeState [] Map.empty

defineLocalVar :: String -> ResolvedProgramM MutableRef
defineLocalVar varName = do
  let ref = LocalVarReference varName
  
  modify (insertRef ref)

  return ref
  where
    insertRef :: MutableRef -> ScopePtr -> ScopePtr
    insertRef ref =
      updateInnerState (\ss -> ss {
                           ssLocalTable = Map.insert
                             varName
                             ref
                             (ssLocalTable ss)
                           })
getResolvedProgram :: ResolvedProgramM NRAST
                   -> Either CompileError ResolvedProgram
getResolvedProgram (ResolvedProgramM computation) = do
  ast <- eAST
  let (ScopeState constants' localTable) = innerScope scopePtr

  let scope =
        scopeFromLocalTable ast constants' localTable

  return (ResolvedProgram scope)
  where
    (eAST, scopePtr) =
      runState (runExceptT computation) initial

    scopeFromLocalTable :: NRAST
                        -> [Either PyExpr Function]
                        -> LookupTable
                        -> Scope
    scopeFromLocalTable ast' consts localTable = Scope
      { scopeAST = ast'
      , scopeConstants = (fmap recConstants) <$> consts
      , scopeLocalVariables = Map.filter isLocal localTable
      , scopeCellVariables = Map.filter isCell localTable
      , scopeFreeVariables = Map.filter isFree localTable
      }

    initial :: ScopePtr
    initial = Top $ ScopeState
      { ssConstants = []
      , ssLocalTable = Map.empty
      }

    recConstants :: Function -> Scope
    recConstants (Function ast ss) =
      scopeFromLocalTable ast (ssConstants ss) (ssLocalTable ss)

-- compileToCodeStruct :: ResolvedProgram -> Either CompileError CodeStruct
-- compileToCodeStruct (ResolvedProgram ast constants' localVars) = do
--   Struct.getCodeStruct compile

--   where
--     compile :: CodeStructM ()
--     compile = do
--       registerConstants
--       registerLocalVariables
--       compileAST ast

--     registerConstants :: CodeStructM ()
--     registerConstants = forM_ constants' Struct.addConstant

--     registerLocalVariables :: CodeStructM ()
--     registerLocalVariables = forM_ localVars Struct.addVariable

--     compileAST :: RAST -> CodeStructM ()
--     compileAST (FAtom _) = return ()
--     compileAST (FReference _) = return ()

--     compileAST (FBegin rest) = do
--       forM_ rest compileAST

--     compileAST (FDefine ref expr) = do
--       evaluateAndPutOnTop expr
--       storeTopOn ref

--     compileAST _ = throwError (NotImplemented "function calls")

--     -- compileAST (FApply (ASymbol "print" : rest)) =
--     --   forM_  rest $ \expr -> do
--     --       evaluateAndPutOnTop expr
--     --       Struct.addInstr PRINT_EXPR

--     evaluateAndPutOnTop :: RAST -> CodeStructM ()
--     evaluateAndPutOnTop (FAtom ref) = putRefOnTop ref
--     evaluateAndPutOnTop (FReference ref) = putRefOnTop ref
--     evaluateAndPutOnTop (FBegin rest) = do
--       putLastOnTop rest
--       where
--         putLastOnTop :: [RAST] -> CodeStructM ()
--         putLastOnTop [] = return ()
--         putLastOnTop [x] = evaluateAndPutOnTop x
--         putLastOnTop (x:xs) = compileAST x >> putLastOnTop xs
    
--     evaluateAndPutOnTop a@(FDefine ref _) = do
--       compileAST a
--       putRefOnTop ref

--     evaluateAndPutOnTop _ = throwError (NotImplemented "calls")

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
