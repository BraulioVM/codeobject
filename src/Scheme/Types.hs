{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Scheme.Types where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
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

data AllowedLambda = NotAllowed | Allowed

data StandardForm (allowed :: AllowedLambda) reference constant where
  FAtom :: constant -> StandardForm allowed reference constant
  FReference :: reference -> StandardForm allowed reference constant

  FBegin :: [StandardForm allowed reference constant]
         -> StandardForm allowed reference constant
  
  FDefine :: reference
          -> StandardForm allowed reference constant
          -> StandardForm allowed reference constant

  FApply :: StandardForm allowed reference constant
         -> [StandardForm allowed reference constant]
         -> StandardForm allowed reference constant

  FLambda :: [reference]
          -> StandardForm 'Allowed reference constant
          -> StandardForm 'Allowed reference constant

deriving instance (Show a) => Show (AbstractProgram a)
deriving instance (Show ref, Show const) => Show (StandardForm k ref const)
deriving instance (Eq ref, Eq const) => Eq (StandardForm k ref const)

instance Bifunctor (StandardForm a) where
  --first :: ref -> ref' -> (StandardForm ref a) -> (StandardForm ref' a)
  first _ (FAtom constant) = FAtom constant
  first f (FReference ref) = FReference (f ref)
  first f (FBegin exprs) = FBegin (first f <$> exprs)
  first f (FDefine ref expr) = FDefine (f ref) (first f expr)
  first f (FApply funcExpr args) =
    FApply (first f funcExpr) (first f <$> args)
  first f (FLambda refs expr) =
    FLambda (f <$> refs) (first f expr)

  second f (FAtom constant) = FAtom (f constant)
  second _ (FReference ref) = FReference ref
  second f (FBegin exprs) = FBegin (second f <$> exprs)
  second f (FDefine ref expr) = FDefine ref (second f expr)
  second f (FApply funcExpr args) =
    FApply (second f funcExpr) (second f <$> args)
  second f (FLambda refs expr) =
    FLambda refs (second f expr)

instance Bifoldable (StandardForm a) where
  bifoldMap _ g (FAtom constant) = g constant
  bifoldMap f _ (FReference ref) = f ref
  bifoldMap f g (FBegin exprs) =
    mconcat (bifoldMap f g <$> exprs)
  -- |Evaluating expr first, not sure if relevant
  -- but this is the way it is going to be done for bitraversable
  bifoldMap f g (FDefine ref expr) = 
    bifoldMap f g expr `mappend` f ref

  bifoldMap f g (FApply funcExpr args) = 
    (bifoldMap f g funcExpr) `mappend`
    mconcat (bifoldMap f g <$> args)

  bifoldMap f g (FLambda args expr) =
    mconcat (f <$> args) `mappend` bifoldMap f g expr

instance Bitraversable (StandardForm a) where
  bitraverse _ g (FAtom constant) = FAtom <$> g constant
  bitraverse f _ (FReference ref) = FReference <$> f ref
  bitraverse f g (FBegin exprs) =
    FBegin <$> sequenceA (bitraverse f g <$> exprs)
  bitraverse f g (FDefine ref expr) =
    flip FDefine <$> bitraverse f g expr <*> f ref
  bitraverse f g (FApply funcExpr args) =
    FApply <$>
    bitraverse f g funcExpr <*>
    sequenceA (bitraverse f g <$> args)
  bitraverse f g (FLambda refs expr) =
    FLambda <$> sequenceA (f <$> refs) <*> bitraverse f g expr

data CompileError = ReservedWordSyntaxError
                  | UnknownSyntax
                  | IncorrectParameterList
                  | UndefinedVariable String
                  | NotImplemented String
                  | ScopeError
  deriving (Show, Eq)

type AST = AbstractProgram BasicValue
type FAST = StandardForm 'Allowed String BasicValue

type NamedAST = StandardForm 'NotAllowed String ConstReference




