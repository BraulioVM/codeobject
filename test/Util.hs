module Util where

import Control.Monad (unless)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Test.HUnit

import Output
import Types

testDirectory :: FilePath
testDirectory = "test" </> "testfiles"

testFile :: FilePath -> FilePath
testFile = (testDirectory </>)

withDirectory :: FilePath -> IO () -> IO ()
withDirectory fp action = do
  exists <- doesDirectoryExist fp
  unless exists (createDirectory fp)
  action
  removeDirectoryRecursive fp

withCreatePythonFiles :: CodeObject -> (FilePath -> IO ()) -> IO ()
withCreatePythonFiles codeObject action = do
  withDirectory tmpDirectory $ do
    compileToFiles tmpDirectory modName codeObject
    writeFile importerPath importerCode
  
    action importerPath
  where
    tmpDirectory :: FilePath
    tmpDirectory = (testFile "mods")
  
    importerPath :: FilePath
    importerPath = tmpDirectory </> "importer.py"

    modName :: String
    modName = "mod"

    importerCode :: String
    importerCode = "import " ++ modName

createAndImport :: CodeObject -> (String -> IO ()) -> IO ()
createAndImport co action = do
  withCreatePythonFiles co $ \importerPath -> do
    (exitCode, output, stderr) <- runPython importerPath
    case exitCode of
      ExitSuccess -> action output
      ExitFailure errorCode -> assertFailure $ "Python error " ++
        show errorCode ++ ": " ++ output ++ stderr
      where
        stdin = ""
        runPython :: FilePath -> IO (ExitCode, String, String)
        runPython importerPath =
          readProcessWithExitCode "python3" [importerPath] stdin

testCodeObjectOutput :: CodeObject
                     -> [String]
                     -> Test
testCodeObjectOutput codeObject expected = TestCase $ 
  createAndImport codeObject $ \pythonOutput ->
    assertEqual "Python output" expected (lines pythonOutput)
