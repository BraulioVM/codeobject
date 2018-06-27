module Types where

import Data.ByteString

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

data PExpr = None
