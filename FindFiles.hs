-- Naive implementation
-- http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
module FindFiles (simpleFind) where
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath
getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

_simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

_simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)

simpleFind ::  FilePath -> FilePath -> IO [FilePath]
simpleFind ext directoryPath = _simpleFind (\p -> takeExtension p == "."++ext ) directoryPath
