-- TODO : print out partially covered line and its xml file for furthur checking.
import System.FilePath (takeDirectory)
import Data.List.Split (splitOn)
import Data.List (groupBy, sortBy, isInfixOf)
data Funcinfolist = Funcinfolist { funcName :: String
                                 , lineCount :: Int
                                 , coverageInfoList ::[(Int,Int)]
                                 , xmlFile :: FilePath
                                 , sourceCode ::[(Int,String)]                                   
                                 } deriving (Show,Read)
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

data Outcome = CompleteCoverage | IncompleteCoverage | UnusedFunction | HardwareException | NoTestRequired deriving (Show,Eq,Ord)
eval :: Funcinfolist -> Outcome
eval a
    | allLinesCovered a == True = CompleteCoverage
    | allLinesUncovered a == True = UnusedFunction
    | allHardwareException (coverageInfoList a) (sourceCode a) == True = HardwareException
    | otherwise = IncompleteCoverage


allLinesCovered :: Funcinfolist -> Bool
allLinesCovered a = all (\x -> snd x == 0 ) (coverageInfoList a)

allLinesUncovered :: Funcinfolist -> Bool
allLinesUncovered a = all (\x -> snd x == 2 ) (coverageInfoList a)

readSrcLine :: FilePath -> Int -> IO String
readSrcLine path lineNum = do
  c <- readFile path
  let line = (lines c) !! (lineNum - 1)
  return line

hardwareException :: String -> Bool
hardwareException a =  "HW_HandleException" `isInfixOf` a
allHardwareException :: [(Int,Int)]-> [(Int,String)]  -> Bool
allHardwareException b a = all (==True) $map (\(lineNum,s)-> ((covResult lineNum > 1000||covResult lineNum==1) && (hardwareException s)) || covResult lineNum == 0 ) a
                           where covResult lineNum=snd $head $filter (\x-> fst x==lineNum) b
f :: Funcinfolist -> IO [String]
f a = sequence $map (\(l,r) -> readSrcLine path l) (coverageInfoList a)
      where path = (takeDirectory $xmlFile a) ++ "/_cases.cpp" -- if it is aut file

z :: [[String]] -> [Funcinfolist] -> [Funcinfolist]
z a b = zipWith (\x y -> k x y) a b 

g :: [String] -> [(Int,Int)] -> [(Int,String)]
g a b = zipWith (\x y->(fst y, x )) a b

k :: [String] -> Funcinfolist -> Funcinfolist
k a b = Funcinfolist (funcName b) (lineCount b) (coverageInfoList b) (xmlFile b) sc
      where sc = g a (coverageInfoList b)
        
main = do
  c <- getContents
  
  let funcInfoLists = map (\x -> x ++ [[]]) $((map $splitOn "$") . lines ) c
  let funcInfoLists_ = map (\x -> Funcinfolist  (x!!0) (read (x!!1)) (read (x!!5)) (x!!6) (read (x!!7))) funcInfoLists
  temp <- mapM f funcInfoLists_
  let funcInfoLists__ = z temp funcInfoLists_
  let result = map (\x-> (x,eval x)) $map (foldl1 merge) $groupBy compareInfoList  funcInfoLists__
--  mapM print $sortBy (\x y-> if (snd x) > (snd y) then GT else LT ) $map (\(x,y)->((funcName x,(coverageInfoList x, xmlFile x)),y)) result
  mapM print $sortBy (\x y-> if (snd x) > (snd y) then GT else LT ) $map (\(x,y)->(take 50 $funcName x,y)) result

