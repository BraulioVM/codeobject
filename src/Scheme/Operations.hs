module Scheme.Operations where

import qualified Data.ByteString as BS
import Scheme.References.Types
import Scheme.Types
import Types
import qualified Operations as Op

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
                  , csArgumentNames :: [String]
                  }

compileCS :: CodeStruct -> CodeObject
compileCS (CodeStruct code consts free cell local argNames) = CodeObject
  { argCount = length argNames
  , kwOnlyArgCount = 0
  , nLocals = length local + length cell
  , stackSize = computeStackSize code
  , flags = 0x40
  , codeString = Op.getByteCode (toPyByteCode code)
  , constants = undefined -- PTuple (toPyExpr <$> consts)
  , names = PTuple []
  , varNames = theVarNames
  , filename = ""
  , name = ""
  , firstLineNo = 0
  , lnotab = BS.empty
  , freeVars = PTuple free
  , cellVars = PTuple cell
  }

  where
    computeStackSize :: [Operation] -> Int
    computeStackSize _ = 0

    toPyByteCode = undefined

    theVarNames = undefined
