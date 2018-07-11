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

  case scopePtr of
    Top ss -> do
      let mRef = localLookup varName ss
      case mRef of
        Nothing -> throwError (UndefinedVariable varName)
        Just ref -> return ref

    Inner ss parent -> do
      let mRef = localLookup varName ss
      case mRef of
        Nothing -> do
          case recursiveLookup varName parent of
            Nothing -> throwError (UndefinedVariable varName)
            Just (newPtr, ref) -> do
              put newPtr
              localRegisterVar varName ref
              return ref
        Just ref ->
          return ref

  where
    localLookup :: String -> ScopeState -> Maybe ReferenceScope
    localLookup varN ss =
      let table = ssTable ss
      in Map.lookup varN table

    recursiveLookup :: String
                    -> ScopePtr
                    -> Maybe (ScopePtr, ReferenceScope)
    recursiveLookup varN (Top ss) = do
      case localLookup varN ss of
        Nothing -> Nothing
        Just ref -> Just
          (Top $ updateReferenceScope ss varN ref, FreeScope)
      
    recursiveLookup varN (Inner ss parent) =
      case localLookup varN ss of
        Nothing -> do
          (newParent, ref) <- recursiveLookup varN parent
          return (Inner ss newParent, ref)
        Just ref -> Just
          (flip Inner parent $
            updateReferenceScope ss varN ref, FreeScope)
            
    updateReferenceScope :: ScopeState 
                         -> String
                         -> ReferenceScope
                         -> ScopeState
    updateReferenceScope innerSS varN ref =
      case ref of
        CellScope -> innerSS
        FreeScope -> innerSS
        LocalScope -> 
          let table = ssTable innerSS
              newTable = Map.insert varN CellScope table
          in innerSS { ssTable = newTable }

-- resolveScopes :: FAST -> Either CompileError Scope
-- resolveScopes = 

