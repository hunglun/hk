
import Data.Tree
import System.Process
import System.Environment
import Data.List.Split (splitOn)
import Data.List (nub,sort)
type SymbolName = String
data ScopeInfo = ScopeInfo {symbolName :: SymbolName
                           ,filePath :: FilePath
                            ,index :: String
                           } deriving (Read,Show)

removeDup :: (Eq a, Eq b) => [(a,b)] -> [(a,b)]
removeDup [] = []
removeDup [x] = [x]
removeDup (x:y:xs) = if fst x == fst y then (removeDup (x:xs)) else x:(removeDup (y:xs))

f :: ScopeInfo -> IO (ScopeInfo, [ScopeInfo])
f s = do
  c <- readCreateProcess (shell ("cscope -dL0 " ++ (symbolName s))) ""
  let results = lines c
  let fpath = ((!! 0) . (!! 0) . filter  (\x -> x!!1 == symbolName s) . map (splitOn " ")  ) results
  let callers = filter (\x -> x!!1 `notElem` [symbolName s,"<global>","defined"]) $map ( splitOn " ") results
  let callersUniq = removeDup $map (\x-> (x!!1,x!!0)) callers
  print [symbolName s,index s]
  print (map fst callersUniq)
  print "-------------------------------------------------------------"
  if (length $index s) < 3 then 
      return (ScopeInfo (symbolName s) fpath (index s) , map (\((x,y),z) -> ScopeInfo  x y ((index s)++(show z)) ) (zip callersUniq [1..] ))             
  else
      return (ScopeInfo (symbolName s) fpath (index s) , [])            

ioUnfoldTree :: (ScopeInfo -> IO (ScopeInfo, [ScopeInfo])) -> ScopeInfo -> IO (Tree ScopeInfo)
ioUnfoldTree f s = do
  (x, xs) <- f s
  forest <- ioUnfoldForest f xs
  return (Node x forest)
  
ioUnfoldForest :: (ScopeInfo -> IO (ScopeInfo, [ScopeInfo])) -> [ScopeInfo] -> IO [Tree ScopeInfo]
ioUnfoldForest f bs = sequence $map (ioUnfoldTree f) bs

main = do
  args <- getArgs
  result <- ioUnfoldTree f (ScopeInfo (args!!0) "" "0")
  print ""
