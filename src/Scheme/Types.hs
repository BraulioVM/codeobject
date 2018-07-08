{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.Types where

import Control.Monad.State
import Data.Map (Map)

import Scheme.References.Types
import Types
import Operations

data BasicValue = AInt Int
                | AString String
               deriving (Show)

instance ToPyExpr BasicValue where
  toPyExpr (AInt x) = PyInt x
  toPyExpr (AString s) = PyString s

data AbstractProgram a = Atom a
                       | ASymbol String
                       | List [AbstractProgram a]

deriving instance (Show a) => Show (AbstractProgram a)

type AST = AbstractProgram BasicValue
type ResolvedAST = AbstractProgram Reference




