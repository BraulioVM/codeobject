{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Scheme.References.Internal where

data ReferenceType = Writable | ReadOnly
  deriving (Eq, Ord, Show)

data AReference index referenceType where
  LocalVarReference :: index -> AReference index 'Writable
  --GlobalVarReference :: index -> AReference index 'Writable
  CellVarReference :: index -> AReference index 'Writable
  FreeVarReference :: index -> AReference index 'Writable
  ConstantVarReference :: index -> AReference index 'ReadOnly
  
deriving instance Show index => Show (AReference index a)
deriving instance Eq index => Eq (AReference index a)

type Reference a = AReference Int a
type NReference a = AReference String a


--deriving instance Ord (Reference a)
