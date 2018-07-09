{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
module Scheme.Types where

import Scheme.References.Types
import Types

data BasicValue = AInt Int
                | AString String
               deriving (Show, Eq)

instance ToPyExpr BasicValue where
  toPyExpr (AInt x) = PyInt x
  toPyExpr (AString s) = PyString s

data AbstractProgram a = Atom a
                       | ASymbol String
                       | List [AbstractProgram a]

data StandardForm reference constant
  = FAtom constant
  | FReference reference
  | FBegin [StandardForm reference constant]
  | FDefine reference (StandardForm reference constant)
  | FApply reference [StandardForm reference constant]
  | FLambda [reference] (StandardForm reference constant)

deriving instance (Show a) => Show (AbstractProgram a)
deriving instance (Show ref, Show const) => Show (StandardForm ref const)
deriving instance (Eq ref, Eq const) => Eq (StandardForm ref const)

data CompileError = ReservedWordSyntaxError
                  | UnknownSyntax
                  | IncorrectParameterList
                  | UndefinedVariable String
                  | NotImplemented String
  deriving (Show, Eq)

type AST = AbstractProgram BasicValue
type FAST = StandardForm String BasicValue

type RAST = StandardForm (Reference 'Writable) (Reference 'ReadOnly)
type ResolvedAST = AbstractProgram (Reference 'ReadOnly)




