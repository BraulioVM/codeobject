{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Word
import Control.Monad (unless)
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.IO hiding (withFile)
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.HUnit

import Operations
import Types
import Marshal
import PyMonad

testDirectory :: FilePath
testDirectory = "test" </> "testfiles"

testFile :: FilePath -> FilePath
testFile = (testDirectory </>)

withFile :: FilePath -> String -> IO () -> IO ()
withFile fp content action = do
  writeFile filename content
  action
  removeFile filename
    where
      filename = testFile fp

withBFile :: FilePath -> ByteString -> IO () -> IO ()
withBFile fp content action = do
  BS.writeFile filename content
  action
  removeFile filename
    where
      filename = testFile fp

withDirectory :: FilePath -> IO () -> IO ()
withDirectory fp action = do
  exists <- doesDirectoryExist path
  unless exists (createDirectory path)
  action
  removeDirectory path
    where
      path = testFile fp

getModifTime :: FilePath -> IO UTCTime
getModifTime = getModificationTime . testFile

withCreatePythonFiles :: String -> CodeObject -> IO () -> IO ()
withCreatePythonFiles modName co action = do
  withFile modFilename "" $ do
    withFile importer "import mod" $ do
      withDirectory pycache $ do
        -- construct a PycFile
        modifTime <- getModifTime modFilename
        let (posixTime :: POSIXTime) = utcTimeToPOSIXSeconds modifTime
            pycFile = PycFile (floor posixTime) co

        withBFile pycFilename (marshal pycFile) action
      
  where
    modFilename = modName ++ ".py"
    importer = "importer.py"
    pycache =  "__pycache__"
    pycFilename = pycache </> modName ++ ".cpython-35.pyc"

createAndImport :: CodeObject -> (String -> IO ()) -> IO ()
createAndImport co action = do
  withCreatePythonFiles "mod" co $ do
    (exitCode, output, stderr) <- runPython
    case exitCode of
      ExitSuccess ->  action output
      ExitFailure errorCode -> assertFailure $ "Python error " ++
        show errorCode ++ ": " ++ output ++ stderr
      where
        args = [testFile "importer.py"]
        stdin = ""
        runPython = readProcessWithExitCode "python3" args stdin

loadConstantObject :: CodeObject
loadConstantObject = defaultObject {
  codeString = getByteCode [
      LOAD_CONST 1,
      PRINT_EXPR,
      LOAD_CONST 0,
      RETURN_VALUE
      ],
   constants = PTuple [PyNone, PyInt 3]
}


testBasic = TestCase $ do
  createAndImport loadConstantObject $ \output ->
    assertEqual "Python output" output "3\n"

binaryAddObject = defaultObject {
  codeString = getByteCode [
      LOAD_CONST 1,
      LOAD_CONST 2,
      BINARY_ADD,
      PRINT_EXPR,
      LOAD_CONST 0,
      RETURN_VALUE
      ],
    constants = PTuple [PyNone, PyInt 5, PyInt 6]
  }

testIntegerAdd = TestCase $ do
  createAndImport binaryAddObject $ \output ->
    assertEqual "Python output" output "11\n"

localVarNames = defaultObject {
  nLocals = 1,
  codeString = getByteCode [
      LOAD_CONST 1,
      STORE_FAST 1,
      LOAD_FAST 1,
      PRINT_EXPR,
      LOAD_CONST 0,
      RETURN_VALUE
      ],
    constants = PTuple [PyNone, PyInt 4]
  }

testLocalVars = TestCase $ do
  createAndImport localVarNames $ \output ->
    assertEqual "Python output" output "4\n"

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

testStrings = TestCase $ do
  createAndImport useStrings $ \output ->
    assertEqual "Python output" output "'hñɊeyho'\n"

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

testJumpForward = TestCase $ do
  createAndImport jumpForward $ \output ->
    assertEqual "Python output" output "13\n"

testJumpIfBoolean = TestCase $ do
  createAndImport jumpIfBoolean $ \output ->
    assertEqual "Python output" output "100\n100\n"

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

testBasicComparisons = TestCase $ do
  createAndImport comparisonOperations $ \output ->
    assertEqual "Python output" output "True\nFalse\nFalse\nTrue\n"

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

testCallFunction = TestCase $ do
  createAndImport callFunctions $ \output ->
    assertEqual "Python output" output "400\n"

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

testMakeFunction = TestCase $ do
  createAndImport createFunction $ \output ->
    assertEqual "Python output" output "2448\n"

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


testPyMonad = TestCase $ do
  createAndImport co $ \output ->
    assertEqual "Python output" output "30\n'hey'\n'hey'\n30\n"
  where
    co = createCodeObject $ do
      x <- defConstant $ PyInt 30
      y <- defConstant $ PyString "hey"
      printRef x
      printRef y
      printRef y
      printRef x

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
  , TestLabel "basic pymonad" testPyMonad
  ]

main = do
  withDirectory "" (const () <$> runTestTT tests)
