{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module Scheme.References.Internal where

data ReferenceType = Writable | ReadOnly
  deriving (Show)

data Reference referenceType where
  LocalVarReference :: Int -> Reference 'Writable
  ConstantVarReference :: Int -> Reference 'ReadOnly

deriving instance Show (Reference a)
