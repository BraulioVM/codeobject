module Scheme.Operations where

import Scheme.References.Types
import Scheme.Types

data Operation = LoadVar MutableRef
               | SaveVar MutableRef
               | LoadConst ConstReference
               | BinaryAdd
               | ReturnValue
               | PrintExpr
               | CallFunction 

data CodeStruct = CodeStruct
                  { csCode :: [Operation]
                  , csConstants :: [Either CodeStruct BasicValue]
                  , csFree :: [String]
                  , csCell :: [String]
                  , csLocal :: [String]
                  }

  
