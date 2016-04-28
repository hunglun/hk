import Data.List (groupBy,maximumBy)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
-- IoMapDvrSafetyBase::BuildCompleteOpenRequest,Lines covered: 0,Lines partially covered 0,Lines not covered: 24,
-- [0,0,24]
covFormat :: String -> [Int]
covFormat =  map (read . dropWhile ( not . isDigit) ) . drop 1 . splitOn ","

-- Get the line with the highest lines covered.
-- in that line, if the lines not covered + lines partially covered ==0, return True , otherwise False
covComplete :: [[Int]] -> Bool
covComplete = (\lst -> lst!!1 == 0 && lst!!2 == 0) . maximumBy (\a b -> if a!!0 >= b!!0 then GT else LT)

covAnalyse = covComplete . map covFormat
main = do
  c <- getContents
  let funcResultList = (groupBy (\x y -> (splitOn "," x) !!0 == (splitOn "," y)!!0) . lines) c
  putStrLn "---------------------------------------- CODE COVERAGE SUMMARY --------------------------------"
  let result = map (\x -> (takeWhile (/=',') (x!!0),covAnalyse x)) funcResultList
  putStrLn "== Complete Code Coverage =="
  mapM putStrLn $map fst $filter ((== True) . snd) result
  putStrLn "== Incomplete Code Coverage =="
  mapM putStrLn $map fst $filter ((== False) . snd) result
  putStrLn "---------------------------------------- END OF SUMMARY ----------------------------------------"
