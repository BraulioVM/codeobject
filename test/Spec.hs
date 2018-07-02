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


test1 = TestCase $ do
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

test2 = TestCase $ do
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

test3 = TestCase $ do
  createAndImport localVarNames $ \output ->
    assertEqual "Python output" output "4\n"

tests = TestList
  [ TestLabel "basic .pyc making" test1
  , TestLabel "binary_add object" test2
  , TestLabel "store_fast and load_fast" test3
  ]

main = do
  withDirectory "" (const () <$> runTestTT tests)
