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
  codeString = getByteCode [LOAD_CONSTANT 0, RETURN_VALUE],
  constants = PTuple [None],
  names = PTuple [],
  varNames = PTuple [],
  filename = "mod.py",
  name = "<module>",
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
