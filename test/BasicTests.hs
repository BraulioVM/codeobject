{-# LANGUAGE ScopedTypeVariables #-}
module BasicTests where

import Data.Word
import Test.HUnit

import Util

import Operations
import Types

testBasic :: Test
testBasic = testCodeObjectOutput loadConstantObject ["3"]
  where
    loadConstantObject :: CodeObject
    loadConstantObject = defaultObject
      { codeString = getByteCode
                   [ LOAD_CONST 1
                   , PRINT_EXPR
                   , LOAD_CONST 0
                   , RETURN_VALUE
                   ]
      , constants = PTuple [PyNone, PyInt 3]
      }

testIntegerAdd :: Test
testIntegerAdd = testCodeObjectOutput binaryAddObject ["11"]
  where
    binaryAddObject = defaultObject
      { codeString = getByteCode 
                     [ LOAD_CONST 1 -- 5 
                     , LOAD_CONST 2 -- 6
                     , BINARY_ADD
                     , PRINT_EXPR
                     , LOAD_CONST 0
                     , RETURN_VALUE
                     ]
      , constants = PTuple [PyNone, PyInt 5, PyInt 6]
      }

testLocalVars :: Test
testLocalVars = testCodeObjectOutput localVarNames ["4"]
  where
    localVarNames :: CodeObject
    localVarNames = defaultObject
      { nLocals = 1
      , codeString = getByteCode
                     [ LOAD_CONST 1 -- 4
                     , STORE_FAST 1
                     , LOAD_FAST 1
                     , PRINT_EXPR
                     , LOAD_CONST 0
                     , RETURN_VALUE
                     ]
      , constants = PTuple [PyNone, PyInt 4]
      }

testStrings :: Test
testStrings = testCodeObjectOutput useStrings ["'hñɊeyho'"]
  where
    useStrings :: CodeObject
    useStrings = defaultObject
      { nLocals = 0
      , stackSize = 3
      , codeString = getByteCode
        [ LOAD_CONST 1,
          LOAD_CONST 2,
          BINARY_ADD,
          PRINT_EXPR,
          LOAD_CONST 0,
          RETURN_VALUE
        ]
      , constants = PTuple [ PyNone
                           , PyString "hñɊey" -- unicode strings
                           , PyString "ho"
                           ]
      }

testJumpForward :: Test
testJumpForward = testCodeObjectOutput jumpForward ["13"]
  where
    jumpForward :: CodeObject
    jumpForward = defaultObject
      { codeString = getByteCode
                     [ LOAD_CONST 1
                     , JUMP_FORWARD 3 -- LOAD_CONST uses 3 bytes
                     , LOAD_CONST 2
                     , PRINT_EXPR
                     , LOAD_CONST 0
                     , RETURN_VALUE
                     ]
      , constants = PTuple [PyNone, PyInt 13, PyInt 41]
      }

testJumpIfBoolean :: Test
testJumpIfBoolean = testCodeObjectOutput jumpIfBoolean ["100", "100"]
  where
    jumpIfBoolean :: CodeObject
    jumpIfBoolean = defaultObject
      { codeString = getByteCode
                     [ LOAD_CONST 3 -- 100 | 0 
                     , LOAD_CONST 1 -- True | 3
                     , POP_JUMP_IF_TRUE 12 -- x | 6
                     , LOAD_CONST 4 -- 200 | 9
                     , PRINT_EXPR -- prints 100 | 12
                     , LOAD_CONST 2 -- False | 15
                     , POP_JUMP_IF_TRUE 10000 -- would crash
                     , LOAD_CONST 3 -- 100 | 19
                     , LOAD_CONST 2 -- False | 22
                     , POP_JUMP_IF_FALSE 31 -- x | 25
                     , LOAD_CONST 4 -- 200 | 28
                     , PRINT_EXPR -- prints 100 | 31
                     , LOAD_CONST 1 -- True | 32
                     , POP_JUMP_IF_FALSE 41 -- x | 35
                     , JUMP_FORWARD 1  -- 1 | 38
                     , PRINT_EXPR -- won't execute | 41
                     , LOAD_CONST 0 -- | 42
                     , RETURN_VALUE -- | 45
                     ]
      , constants = PTuple [ PyNone
                           , PyBool True
                           , PyBool False
                           , PyInt 100
                           , PyInt 200
                           ]
      }

testBasicComparisons :: Test
testBasicComparisons =
  testCodeObjectOutput comparisonOperations [ "True"
                                            , "False"
                                            , "False"
                                            , "True" ]
  where
    compareConstants :: Word16
                     -> Word16
                     -> ComparisonOperation
                     -> [Operation]
    compareConstants x y op =
      [ LOAD_CONST x
      , LOAD_CONST y
      , COMPARE_OP op
      , PRINT_EXPR
      ]

    comparisonOperations :: CodeObject
    comparisonOperations = defaultObject
      { codeString = getByteCode $
                     compareConstants 1 2 LESS `mappend`
                     compareConstants 1 2 GREATER `mappend`
                     compareConstants 1 2 EQUAL `mappend`
                     compareConstants 1 1 EQUAL `mappend`
                     [ LOAD_CONST 0
                     , RETURN_VALUE
                     ]
      , constants = PTuple [ PyNone
                           , PyInt 200
                           , PyInt 300
                           ]
      }

testCallFunction :: Test
testCallFunction =
  testCodeObjectOutput callFunctions ["400"]
  where
    callFunctions :: CodeObject
    callFunctions = defaultObject
      { codeString = getByteCode
                     [ LOAD_GLOBAL 0 -- print
                     , LOAD_GLOBAL 1 -- max
                     , LOAD_CONST 1 -- 200
                     , LOAD_CONST 2 -- 400
                     , LOAD_CONST 3 -- 300
                     , CALL_FUNCTION 3 -- max(200, 400, 300)
                     , CALL_FUNCTION 1 -- print(400)
                     , LOAD_CONST 0
                     , RETURN_VALUE
                     ]
                     
      , constants = PTuple [ PyNone
                           , PyInt 200
                           , PyInt 400
                           , PyInt 300
                           ]
      , names = PTuple [ "print"
                       , "max"
                       ]
      }

testMakeFunction :: Test
testMakeFunction =
  testCodeObjectOutput createFunction ["2448"]
  where
    createFunction :: CodeObject
    createFunction = defaultObject
      { codeString = getByteCode
                     [ LOAD_CONST 1 -- code object
                     , LOAD_CONST 2 -- function name
                     , MAKE_FUNCTION 0
                     , LOAD_CONST 3 -- 1337
                     , LOAD_CONST 4 -- 1111
                     , CALL_FUNCTION 2 -- call made function
                     , PRINT_EXPR
                     , LOAD_CONST 0
                     , RETURN_VALUE
                     ]
                     
      , constants = PTuple [ PyNone
                           , PyCodeObject (defaultObject
                                           { argCount = 2
                                           , nLocals = 2
                                           , stackSize = 2
                                           , codeString = getByteCode
                                             [ LOAD_FAST 0
                                             , LOAD_FAST 1
                                             , BINARY_ADD
                                             , RETURN_VALUE
                                             ]
                                           , varNames = PTuple
                                             [ "a"
                                             , "b" ]
                                           , filename = ""
                                           , name = "g"
                                           })
                           , PyString "g"
                           , PyInt 1337
                           , PyInt 1111
                           ]
      }


tests :: Test
tests = TestList
  [ TestLabel "basic .pyc making" testBasic
  , TestLabel "binary_add object" testIntegerAdd
  , TestLabel "store_fast and load_fast" testLocalVars
  , TestLabel "load strings" testStrings
  , TestLabel "jumps forward" testJumpForward
  , TestLabel "boolean jumps" testJumpIfBoolean
  , TestLabel "test basic comparisons" testBasicComparisons
  , TestLabel "basic function calling" testCallFunction
  , TestLabel "basic function making" testMakeFunction
  ]
