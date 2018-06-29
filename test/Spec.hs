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

codeobject :: CodeObject
codeobject = CodeObject {
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


test1 = TestCase $ do
  createAndImport codeobject $ \output ->
    assertEqual "Python output" output "7\n"

tests = TestList [TestLabel "hey" test1]

main = do
  withDirectory testDirectory (const () <$> runTestTT tests)
