{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.ByteString
import Operations

-- | Represents data that will be
-- marshalled as a python tuple.
newtype PTuple a = PTuple [a]

unPTuple :: PTuple a -> [a]
unPTuple (PTuple as) = as

type CodeString = ByteString

-- | Represents python expressions
data PyExpr = PyNone -- ^ None value
            | PyInt Int -- ^ A python integer (fixed-size)
            | PyString String -- ^ A python string
            | PyBool Bool -- ^ A python boolean
            | PyTuple [PyExpr] -- ^ A python tuple
            | PyList [PyExpr] -- ^ A python list
            | PyCodeObject CodeObject -- ^ A python code object

class ToPyExpr a where
  toPyExpr :: a -> PyExpr

instance ToPyExpr PyExpr where
  toPyExpr = id

instance ToPyExpr Int where
  toPyExpr = PyInt

instance ToPyExpr String where
  toPyExpr = PyString

instance ToPyExpr Bool where
  toPyExpr = PyBool

instance ToPyExpr a => ToPyExpr (PTuple a) where
  toPyExpr = PyTuple . Prelude.map toPyExpr . unPTuple

instance ToPyExpr CodeObject where
  toPyExpr = PyCodeObject

-- | Python code object
data CodeObject = CodeObject
  { argCount :: Int -- ^  Arguments of the function (not including
                    -- *args and **kwargs)
  , kwOnlyArgCount :: Int -- ^ Number of keyword-only arguments
  , nLocals :: Int  -- ^ Number of local variables
  , stackSize :: Int -- ^ Stack size required for the function
  , flags :: Int -- ^ Interpreter flags
  , codeString :: CodeString -- ^ Function's bytecode
  , constants :: PTuple PyExpr -- ^ Tuple containing the constants
                              -- used during the execution of the function
  , names :: PTuple String -- ^ Global variable's names used during
                           -- the execution of the function.
  , varNames :: PTuple String -- ^ Local variable's names.
  , filename :: String -- ^ Source filename containing the code
  , name :: String -- ^ The function's name
  , firstLineNo :: Int -- ^ First source file line where the function
                       -- is implemented.
  , lnotab :: ByteString -- ^ Maps source lines to bytecode instructions
                         -- (don't know how).
  , freeVars :: PTuple String -- ^ Variables used in the function
                              -- that are neither local nor global 
  , cellVars :: PTuple String  -- ^ Local variables used by inner functions
  }


-- | A good value to build simple code objects from
defaultObject :: CodeObject
defaultObject = CodeObject {
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
