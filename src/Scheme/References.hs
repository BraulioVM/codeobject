{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scheme.References where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Scheme.Types
import Scheme.References.Types
import Scheme.References.Internal

import Types hiding (name)

data ReferenceScope = LocalScope
                    | FreeScope
                    | CellScope
                    deriving (Show, Eq)

data Scope = Scope
  { scopeCode :: FAST
  , scopeSubScopes :: [Scope]
  , scopeVars :: Map String ReferenceScope    
  } deriving (Show, Eq)

data ScopeState = ScopeState
  { ssTable :: Map String ReferenceScope
  , ssSubScopes :: [Scope]
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

localRegisterVar :: String -> ReferenceScope -> ScopeM ()
localRegisterVar varName refScope = do
  currentScope <- gets innerScope
  let table = ssTable currentScope
      newTable = Map.insert varName refScope table
  
  modify (updateInnerScope (\s -> s { ssTable = newTable }))

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
          put newPtr
          localRegisterVar varName FreeScope
          return FreeScope


  where
    performOnParent :: (ScopePtr -> Maybe a) -> ScopePtr -> Maybe a
    performOnParent f (Inner _ parent) = f parent
    performOnParent f (Top _) = Nothing

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
        localLookupAndTransform ptr = do
          ref <- localLookup varN (innerScope ptr)
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

