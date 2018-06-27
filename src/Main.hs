module Main where

import Types
import Operations
import Marshal

mainCodeObject :: CodeObject
mainCodeObject = CodeObject {
  argCount = 0,
  kwOnlyArgCount = 0,
  nLocals = 0,
  stackSize = 0,
  flags = 0x0040, 
  codeString = getByteCode [
      LOAD_CONSTANT 1,
      LOAD_CONSTANT 2,
      BINARY_ADD,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
      RETURN_VALUE
      ],
  constants = PTuple [PNone, PInt 3, PInt 4],
  names = PTuple [],
  varNames = PTuple [],
  filename = "",
  name = "",
  firstLineNo = 1,
  lnotab = getByteCode [],
  freeVars = PTuple [],
  cellVars = PTuple []
  }

file :: PycFile
file = PycFile mainCodeObject

main :: IO ()
main = do
  writePycFile "../../pruebas/__pycache__/mod.cpython-35.pyc" file
  putStrLn "hello world"
