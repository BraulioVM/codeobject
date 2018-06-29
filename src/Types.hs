module Types where

import Data.ByteString
import Operations

data CodeObject = CodeObject {
  argCount :: Int,
  kwOnlyArgCount :: Int,
  nLocals :: Int,
  stackSize :: Int,
  flags :: Int,
  codeString :: CodeString,
  constants :: PTuple PExpr,
  names :: PTuple String,
  varNames :: PTuple String,
  filename :: String,
  name :: String,
  firstLineNo :: Int,
  lnotab :: ByteString,
  freeVars :: PTuple String,
  cellVars :: PTuple String
  }

data PTuple a = PTuple [a]
type CodeString = ByteString

data PExpr = PNone
           | PInt Int

basicObject :: CodeObject
basicObject = CodeObject {
  argCount = 0,
  kwOnlyArgCount = 0,
  nLocals = 0,
  stackSize = 0,
  flags = 0x0040, 
  codeString = getByteCode [],
  constants = PTuple [],
  names = PTuple [],
  varNames = PTuple [],
  filename = "",
  name = "",
  firstLineNo = 1,
  lnotab = empty,
  freeVars = PTuple [],
  cellVars = PTuple []
  }
