
import Data.Tree
import System.Process
import System.Environment

type SymbolName = String
data ScopeInfo = ScopeInfo {symbolName :: SymbolName
                           ,filePath :: FilePath
                           ,index :: Int} deriving (Read,Show)

f :: SymbolName -> IO (ScopeInfo, [SymbolName])
f s = do
  result <- readCreateProcess (shell ("ls " ++ s)) ""
  return (ScopeInfo s s 0 ,lines result)

ioUnfoldTree :: (SymbolName -> IO (ScopeInfo, [SymbolName])) -> SymbolName -> IO (Tree ScopeInfo)
ioUnfoldTree f s = do
  (x, xs) <- f s
  print x
  forest <- ioUnfoldForest f xs
  return (Node x forest)
ioUnfoldForest :: (SymbolName -> IO (ScopeInfo, [SymbolName])) -> [SymbolName] -> IO [Tree ScopeInfo]
ioUnfoldForest f bs = sequence $map (ioUnfoldTree f) bs

main = do
  print "hello"
  args <- getArgs
  result <- ioUnfoldTree f (args!!0)  
  print result
