-- TODO : print out partially covered line and its xml file for furthur checking.
import System.FilePath (takeDirectory,splitPath)
import Data.List.Split (splitOn)
import System.Directory (doesFileExist)
import Data.List (groupBy, sortBy, isInfixOf)
data Funcinfolist = Funcinfolist { funcName :: String
                                 , lineCount :: Int
                                 , coverageInfoList ::[(Int,Int)]
                                 , xmlFile :: FilePath
                                 , sourceCode ::[(Int,String)]                                   
                                 } deriving (Show,Read)

data Outcome = CompleteCoverage | IncompleteCoverage | UnusedFunction | HardwareException | NoTestRequired deriving (Show,Eq,Ord)

compareFuncBody :: Int -> [Int] -> [Int] -> Bool
compareFuncBody diff a b = all (\x-> fst x - diff == snd x) $zip a b

mergeCoverageInfoList :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
-- 0 represents a covered line; x!!1 * y!!1 merges coverage result from 2 test data sources.
-- partially covered line will show up as a 4 digit number
mergeCoverageInfoList a b = zipWith (\x y-> (fst x, if snd x == 1 then 1000 * snd y
                                                    else if snd y == 1 then 1000 * snd x else snd y * snd x )) a b
merge :: Funcinfolist -> Funcinfolist -> Funcinfolist
merge a b = Funcinfolist (funcName a) (lineCount a) (mergeCoverageInfoList (coverageInfoList a) (coverageInfoList b)) (xmlFile a) (sourceCode a)

compareInfoList :: Funcinfolist -> Funcinfolist -> Bool
compareInfoList a b 
    | funcName a /= funcName b = False
    | lineCount a /= lineCount b = False
    | otherwise = compareFuncBody (listA !!0 - listB!!0 ) listA listB
    where  listA = map fst $coverageInfoList a
           listB = map fst $coverageInfoList b

eval :: Funcinfolist -> Outcome
eval a
    | allLinesCovered a == True = CompleteCoverage
    | allLinesUncovered a == True = UnusedFunction
    | allHardwareException (coverageInfoList a) (sourceCode a) == True = HardwareException
    | otherwise = IncompleteCoverage


allLinesCovered :: Funcinfolist -> Bool
allLinesCovered a = all (\x -> snd x == 0 ) (coverageInfoList a)

allLinesUncovered :: Funcinfolist -> Bool
allLinesUncovered a = all (\x -> snd x >= 2 && snd x < 1000 ) (coverageInfoList a)

readSrcLine :: FilePath -> Int -> IO String
readSrcLine xmlFile lineNum = do
  let f1 = (takeDirectory  xmlFile) ++ "/_cases.cpp"
  let f2 = (takeDirectory  xmlFile) ++ "/_cases.c"      
  let f3 = (takeDirectory  xmlFile) ++ "/unitTest.cpp" 
  let f4 = (takeDirectory  xmlFile) ++ "/unitTest.c"
  existence <- mapM doesFileExist [f1,f2,f3,f4]
  let sourceFile = fst $head $filter (\x -> snd x == True) $zip [f1,f2,f3,f4] existence
  c <- readFile sourceFile
  let line = (lines c) !! (lineNum - 1)
  return line

hardwareException :: String -> Bool
hardwareException a =  "HW_HandleException" `isInfixOf` a
allHardwareException :: [(Int,Int)]-> [(Int,String)]  -> Bool
allHardwareException b a = all (==True) $map (\(lineNum,s)-> ((covResult lineNum > 1000||covResult lineNum==1) && (hardwareException s)) || covResult lineNum == 0 ) a
                           where covResult lineNum=snd $head $filter (\x-> fst x==lineNum) b
generateSourceCode :: Funcinfolist -> IO [String]
generateSourceCode a = sequence $map (\(l,r) -> readSrcLine (xmlFile a) l) (coverageInfoList a)

updateFuncInfoList :: [[String]] -> [Funcinfolist] -> [Funcinfolist]
updateFuncInfoList a b = zipWith (\x y -> updateFuncInfoListHelper x y) a b 

updateFuncInfoListHelper :: [String] -> Funcinfolist -> Funcinfolist
updateFuncInfoListHelper a b = Funcinfolist (funcName b) (lineCount b) (coverageInfoList b) (xmlFile b) sc
      where sc = makeLineSourcePair a (coverageInfoList b)

makeLineSourcePair :: [String] -> [(Int,Int)] -> [(Int,String)]
makeLineSourcePair a b = zipWith (\x y->(fst y, x )) a b

        
main = do
  c <- getContents  
  let funcInfoListsData = map (\x -> x ++ [[]]) $((map $splitOn "$") . lines ) c
  let funcInfoLists = map (\x -> Funcinfolist  (x!!0) (read (x!!1)) (read (x!!5)) (x!!6) (read (x!!7))) funcInfoListsData
  sourceCodeList <- mapM generateSourceCode funcInfoLists
  let updatedFuncInfoLists = updateFuncInfoList sourceCodeList funcInfoLists
  let result = map (\x-> (x,eval x)) $map (foldl1 merge) $groupBy compareInfoList  updatedFuncInfoLists
  mapM print $sortBy (\x y-> if (fst x) > (fst y) then GT else LT ) $map (\(x,y)->(y,(funcName x,(coverageInfoList x, xmlFile x)))) result
--  mapM print $sortBy (\x y-> if (snd x) > (snd y) then GT else LT ) $map (\(x,y)->(take 50 $funcName x,y)) result

