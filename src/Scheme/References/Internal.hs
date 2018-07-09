{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Scheme.References.Internal where

data ReferenceType = Writable | ReadOnly
  deriving (Eq, Ord, Show)

data Reference referenceType where
  LocalVarReference :: Int -> Reference 'Writable
  ConstantVarReference :: Int -> Reference 'ReadOnly

deriving instance Show (Reference a)
deriving instance Eq (Reference a)
deriving instance Ord (Reference a)
