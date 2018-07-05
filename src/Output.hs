module Output where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX
import Data.Time.Clock
import System.Directory
import System.FilePath

import Types
import Marshal

compileToFiles :: FilePath -> String -> CodeObject -> IO ()
compileToFiles fp modName codeObject = do
  createEmptyPyFile
  modificationTime <- getPyFileModTime

  existsCacheDir <- existsPyCacheDir
  unless existsCacheDir createPyCacheDir
  
  createPycFile (floor modificationTime)

  where
    modFilename = fp </> modName ++ ".py"
    cacheDir = fp </> "__pycache__"
    pycFilename = cacheDir </> modName ++ ".cpython-35.pyc"

    createEmptyPyFile :: IO ()
    createEmptyPyFile = do
      writeFile modFilename ""

    getPyFileModTime :: IO POSIXTime 
    getPyFileModTime = do
      modifTime <- getModificationTime modFilename
      return (utcTimeToPOSIXSeconds modifTime)
      
    existsPyCacheDir :: IO Bool
    existsPyCacheDir = doesDirectoryExist cacheDir

    createPyCacheDir :: IO ()
    createPyCacheDir = createDirectory cacheDir

    createPycFile :: Int -> IO ()
    createPycFile timestamp = do
      BS.writeFile pycFilename (marshal $ PycFile timestamp codeObject)

