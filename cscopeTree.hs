
import Data.Tree
import System.Process
import System.Environment
import Data.List.Split (splitOn)
import Data.List (nub,sort)
type SymbolName = String
data ScopeInfo = ScopeInfo {symbolName :: SymbolName
                           ,filePath :: FilePath
                           } deriving (Read,Show)

f :: SymbolName -> IO (ScopeInfo, [SymbolName])
f s = do
  c <- readCreateProcess (shell ("cscope -dL0 " ++ s)) ""
  let results = lines c
  let fpath = ((!! 0) . splitOn " " . (!! 0) ) results
  let callerSymbolList = nub $filter ((`notElem` [s,"<global>"])) $map ((!! 1) . splitOn " ") $drop 1 results
  print s
  print callerSymbolList
  print "--"
  return (ScopeInfo s fpath ,callerSymbolList)

ioUnfoldTree :: (SymbolName -> IO (ScopeInfo, [SymbolName])) -> SymbolName -> IO (Tree ScopeInfo)
ioUnfoldTree f s = do
  (x, xs) <- f s
  forest <- ioUnfoldForest f xs
  return (Node x forest)
ioUnfoldForest :: (SymbolName -> IO (ScopeInfo, [SymbolName])) -> [SymbolName] -> IO [Tree ScopeInfo]
ioUnfoldForest f [] = return []
ioUnfoldForest f bs = sequence $map (ioUnfoldTree f) bs

main = do
  print "hello"
  args <- getArgs
  result <- ioUnfoldTree f (args!!0)  
  print  $(levels result)!!1
