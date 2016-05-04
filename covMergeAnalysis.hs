-- TODO : print out partially covered line and its xml file for furthur checking.
import Data.List.Split (splitOn)
import Data.List (groupBy, sortBy)
data Funcinfolist = Funcinfolist { funcName :: String
                                 , lineCount :: Int
                                 , coverageInfoList ::[(Int,Int)]
                                 , xmlFile :: String} deriving (Show,Read)
compareFuncBody :: Int -> [Int] -> [Int] -> Bool
compareFuncBody diff a b = all (\x-> fst x - diff == snd x) $zip a b

mergeCoverageInfoList :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
-- 0 represents a covered line; x!!1 * y!!1 merges coverage result from 2 test data sources.
-- partially covered line will show up as a 4 digit number
mergeCoverageInfoList a b = zipWith (\x y-> (fst x, if snd x == 1 then 1000 * snd y
                                                    else if snd y == 1 then 1000 * snd x else snd y * snd x )) a b
merge :: Funcinfolist -> Funcinfolist -> Funcinfolist
merge a b = Funcinfolist (funcName a) (lineCount a) (mergeCoverageInfoList (coverageInfoList a) (coverageInfoList b)) (xmlFile a)

compareInfoList :: Funcinfolist -> Funcinfolist -> Bool
compareInfoList a b 
    | funcName a /= funcName b = False
    | lineCount a /= lineCount b = False
    | otherwise = compareFuncBody (listA !!0 - listB!!0 ) listA listB
    where  listA = map fst $coverageInfoList a
           listB = map fst $coverageInfoList b


allLinesCovered :: Funcinfolist -> (Funcinfolist,Bool)
allLinesCovered a = (a,all (\x -> snd x == 0 ) (coverageInfoList a))

main = do
  c <- getContents
  let funcInfoLists = ((map $splitOn "$") . lines ) c
  let result = map allLinesCovered $map (foldl1 merge) $groupBy compareInfoList  (map (\x -> Funcinfolist  (x!!0) (read (x!!1)) (read (x!!5)) (x!!6) ) funcInfoLists)
  mapM print  $filter (\x-> snd x == False ) result
