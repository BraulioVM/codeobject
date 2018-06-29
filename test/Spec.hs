{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.IO hiding (withFile)
import System.Directory
import System.FilePath
import System.Process
import Test.HUnit
import Operations
import Types
import Marshal

withFile :: FilePath -> String -> IO () -> IO ()
withFile fp content action = do
  writeFile fp content
  action
  removeFile fp

withBFile :: FilePath -> ByteString -> IO () -> IO ()
withBFile fp content action = do
  BS.writeFile fp content
  action
  removeFile fp

withDirectory :: FilePath -> IO () -> IO ()
withDirectory fp action = do
  exists <- doesDirectoryExist fp
  unless exists (createDirectory fp)
  action
  removeDirectory fp

testDirectory :: FilePath
testDirectory = "test" </> "testfiles"

testFile :: FilePath -> FilePath
testFile = (testDirectory </>)

withCreatePythonFiles :: String -> CodeObject -> IO () -> IO ()
withCreatePythonFiles modName co action = do
  withFile modFilename "" $ do
    withFile importer "import mod" $ do
      withDirectory pycache $ do
        -- construct a PycFile
        modifTime <- getModificationTime modFilename
        let (posixTime :: POSIXTime) = utcTimeToPOSIXSeconds modifTime
            pycFile = PycFile (floor posixTime) co

        withBFile pycFilename (marshal pycFile) action
      
  where
    modFilename = testFile $ modName ++ ".py"
    importer = testFile "importer.py"
    pycache = testFile "__pycache__"
    pycFilename = pycache </> modName ++ ".cpython-35.pyc"

createAndImport :: CodeObject -> (String -> IO ()) -> IO ()
createAndImport co action = do
  withCreatePythonFiles "mod" co $ do
    output <- readProcess "python3" [testFile "importer.py"] "" 
    action output
    return ()

loadConstantObject :: CodeObject
loadConstantObject = basicObject {
  codeString = getByteCode [
      LOAD_CONSTANT 1,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
      RETURN_VALUE
      ],
   constants = PTuple [PNone, PInt 3]
}


test1 = TestCase $ do
  createAndImport loadConstantObject $ \output ->
    assertEqual "Python output" output "3\n"

binaryAddObject = basicObject {
  codeString = getByteCode [
      LOAD_CONSTANT 1,
      LOAD_CONSTANT 2,
      BINARY_ADD,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
      RETURN_VALUE
      ],
    constants = PTuple [PNone, PInt 5, PInt 6]
  }

test2 = TestCase $ do
  createAndImport binaryAddObject $ \output ->
    assertEqual "Python output" output "11\n"

localVarNames = basicObject {
  nLocals = 1,
  codeString = getByteCode [
      LOAD_CONSTANT 1,
      STORE_FAST 1,
      LOAD_FAST 1,
      PRINT_EXPR,
      LOAD_CONSTANT 0,
      RETURN_VALUE
      ],
    constants = PTuple [PNone, PInt 4]
  }

test3 = TestCase $ do
  createAndImport localVarNames $ \output ->
    assertEqual "Python output" output "4\n"


tests = TestList [
  TestLabel "basic .pyc making" test1,
  TestLabel "binary_add object" test2,
  TestLabel "store_fast and load_fast" test3
  ]

main = do
  withDirectory testDirectory (const () <$> runTestTT tests)
