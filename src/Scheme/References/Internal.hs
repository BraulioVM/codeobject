module Scheme.References.Internal where

data Reference = LocalVarReference Int
               | ConstantVarReference Int
