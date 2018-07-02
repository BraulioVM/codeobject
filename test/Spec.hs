{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Control.Monad (unless)
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
      LOAD_CONSTANT 1,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
      RETURN_VALUE
      ],
   constants = PTuple [PyNone, PyInt 3]
}


testBasic = TestCase $ do
  createAndImport loadConstantObject $ \output ->
    assertEqual "Python output" output "3\n"

binaryAddObject = defaultObject {
  codeString = getByteCode [
      LOAD_CONSTANT 1,
      LOAD_CONSTANT 2,
      BINARY_ADD,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
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
      LOAD_CONSTANT 1,
      STORE_FAST 1,
      LOAD_FAST 1,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
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
    [ LOAD_CONSTANT 1,
      LOAD_CONSTANT 2,
      BINARY_ADD,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
      RETURN_VALUE
    ]
  , constants = PTuple [PyNone, PyString "hey", PyString "ho"]
  }

testStrings = TestCase $ do
  createAndImport useStrings $ \output ->
    assertEqual "Python output" output "'heyho'\n"

jumpForward :: CodeObject
jumpForward = defaultObject
  { codeString = getByteCode
                 [ LOAD_CONSTANT 1
                 , JUMP_FORWARD 3 -- LOAD_CONSTANT uses 3 bytes
                 , LOAD_CONSTANT 2
                 , PRINT_EXPR
                 , LOAD_CONSTANT 0
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
                     [ LOAD_CONSTANT 3 -- 100 | 0 
                     , LOAD_CONSTANT 1 -- True | 3
                     , POP_JUMP_IF_TRUE 12 -- x | 6
                     , LOAD_CONSTANT 4 -- 200 | 9
                     , PRINT_EXPR -- prints 100 | 12
                     , LOAD_CONSTANT 2 -- False | 15
                     , POP_JUMP_IF_TRUE 10000 -- would crash
                     , LOAD_CONSTANT 3 -- 100 | 19
                     , LOAD_CONSTANT 2 -- False | 22
                     , POP_JUMP_IF_FALSE 31 -- x | 25
                     , LOAD_CONSTANT 4 -- 200 | 28
                     , PRINT_EXPR -- prints 100 | 31
                     , LOAD_CONSTANT 1 -- True | 32
                     , POP_JUMP_IF_FALSE 41 -- x | 35
                     , JUMP_FORWARD 1  -- 1 | 38
                     , PRINT_EXPR -- won't execute | 41
                     , LOAD_CONSTANT 0 -- | 42
                     , RETURN_VALUE -- | 45
                     ]
      , constants = PTuple [ PyNone
                           , PyBool True
                           , PyBool False
                           , PyInt 100
                           , PyInt 200
                           ]
      }

tests = TestList
  [ TestLabel "basic .pyc making" testBasic
  , TestLabel "binary_add object" testIntegerAdd
  , TestLabel "store_fast and load_fast" testLocalVars
  , TestLabel "load strings" testStrings
  , TestLabel "jumps forward" testJumpForward
  , TestLabel "boolean jumps" testJumpIfBoolean
  ]

main = do
  withDirectory "" (const () <$> runTestTT tests)
