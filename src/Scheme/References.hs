{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scheme.References where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

import Scheme.Types
import Scheme.References.Types
import Scheme.References.Internal

data ReferenceScope = LocalScope
                    | FreeScope
                    | CellScope
                    deriving (Show, Eq)

data Scope = Scope
  { scopeCode :: NamedAST
  , scopeConstants :: [Either Scope BasicValue]
  , scopeVars :: Map String ReferenceScope    
  } deriving (Show, Eq)

data ScopeState = ScopeState
  { ssTable :: Map String ReferenceScope
  , ssConstants :: [Either Scope BasicValue]
  } deriving (Show, Eq)

data ScopePtr = Top ScopeState | Inner ScopeState ScopePtr
  deriving (Show, Eq)

innerScope :: ScopePtr -> ScopeState
innerScope (Top ss) = ss
innerScope (Inner ss _) = ss

updateInnerScope :: (ScopeState -> ScopeState)
                 -> (ScopePtr -> ScopePtr)
updateInnerScope f (Top ss) = Top (f ss)
updateInnerScope f (Inner ss parent) = Inner (f ss) parent

newtype ScopeM a =
  ScopeM (ExceptT CompileError (State ScopePtr) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState ScopePtr
           , MonadError CompileError
           )

getResolvedProgram :: ScopeM NamedAST -> Either CompileError Scope
getResolvedProgram (ScopeM act) = do
  fast <- eFast

  case ptr of
    Inner _ _ -> throwError (A "get resolved program")
    Top ss -> 
      return $ Scope
      { scopeCode = fast
      , scopeConstants = ssConstants ss
      , scopeVars = ssTable ss
      }
  
  where
    (eFast, ptr) = runState (runExceptT act) initial
    initial = Top (ScopeState Map.empty [])

localRegisterVar :: String -> ReferenceScope -> ScopeM ()
localRegisterVar varName refScope = do
  currentScope <- gets innerScope
  let table = ssTable currentScope
      newTable = Map.insert varName refScope table
  
  modify (updateInnerScope (\s -> s { ssTable = newTable }))

defineSubScope :: [String] -> ScopeM NamedAST -> ScopeM ConstReference
defineSubScope _ action = do
  modify (Inner $ ScopeState { ssTable = Map.empty
                             , ssConstants = [] })
  fast <- action
  inner <- gets innerScope

  let scope = Scope
        { scopeCode = fast
        , scopeConstants = ssConstants inner
        , scopeVars = ssTable inner
        }
  removeInnerScope
  addConstant (Left scope)
  
removeInnerScope :: ScopeM ()
removeInnerScope = do
  ptr <- get

  case ptr of
    Top _ -> throwError (A "removeInnerScope")
    (Inner _ parent) -> put parent

addConstant :: Either Scope BasicValue -> ScopeM ConstReference
addConstant ct = do
  ss <- gets innerScope

  let scps = ssConstants ss
      newScps = scps ++ [ct]
      newSS = ss { ssConstants = newScps }
      newConstID = length scps
  
  modify (updateInnerScope (const newSS))

  return (ConstantVarReference newConstID)

requireVar :: String -> ScopeM ReferenceScope
requireVar varName = do
  scopePtr <- get
  let mLocalRef = localLookup varName (innerScope scopePtr)
  case mLocalRef of
    Just ref ->
      return ref
    Nothing -> do
      case performOnParent (recursiveLookup' varName) scopePtr of
        Nothing -> throwError (UndefinedVariable varName)
        Just newPtr -> do
          put (Inner (innerScope scopePtr) newPtr)
          localRegisterVar varName FreeScope
          return FreeScope

  where
    performOnParent :: (ScopePtr -> Maybe a) -> ScopePtr -> Maybe a
    performOnParent f (Inner _ parent) = f parent
    performOnParent _ (Top _) = Nothing

    foldUp :: (Alternative f) => (ScopePtr -> f ScopePtr)
           -> ScopePtr -> f ScopePtr
    foldUp f ptr@(Top _) = f ptr
    foldUp f ptr@(Inner ss parent) = f ptr <|>
                                     (Inner ss <$> foldUp f parent)

    
    recursiveLookup' :: String -> ScopePtr -> Maybe ScopePtr
    recursiveLookup' varN ptr =
      foldUp localLookupAndTransform ptr
      where
        localLookupAndTransform :: ScopePtr
                                -> Maybe ScopePtr
        localLookupAndTransform ptr' = do
          ref <- localLookup varN (innerScope ptr')
          case ref of
            LocalScope -> return (updateInnerScope markCellVar ptr)
            _ -> return ptr
          
        markCellVar :: ScopeState -> ScopeState
        markCellVar ss  =
          let table = ssTable ss
              newTable = Map.insert varN CellScope table
          in ss { ssTable = newTable  }


    localLookup :: String -> ScopeState -> Maybe ReferenceScope
    localLookup varN ss =
      let table = ssTable ss
      in Map.lookup varN table

-- resolveScopes :: FAST -> Either CompileError Scope
-- resolveScopes = 


data IndexedScope = IndexedScope
  { ixsCode :: IndexedAST
  , ixsConstants :: [Either IndexedScope BasicValue]
  , ixsFree :: [String]
  , ixsCell :: [String]
  , ixsLocal :: [String]
  }

shrinkScope :: Scope -> IndexedScope
shrinkScope (Scope code consts varTable) =
  IndexedScope
  { ixsCode = newCode
  , ixsConstants = newConstants
  , ixsFree = freeVars
  , ixsCell = cellVars
  , ixsLocal = localVars
  }
  where
    freeVars = Map.keys (Map.filter (==FreeScope) varTable)
    cellVars = Map.keys (Map.filter (==CellScope) varTable)
    localVars = Map.keys (Map.filter (==LocalScope) varTable)
    newConstants = first shrinkScope <$> consts
    
    (Just newCode) = bitraverse (lookReferences varTable) pure code

    lookReferences :: Map String ReferenceScope
                   -> String
                   -> Maybe (MutableRef)
    lookReferences varTable' key = do
      varScope <- Map.lookup key varTable'
      case varScope of
        LocalScope ->
          LocalVarReference <$> findIndex (==key) localVars
        CellScope ->
          CellVarReference <$> findIndex (==key) cellVars
        FreeScope ->
          FreeVarReference <$> findIndex (==key) freeVars
