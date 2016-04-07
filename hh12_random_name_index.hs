import System.Random
-- Other Useful Functions:
-- randoms
-- radom

-- when the program starts, a new generator is made.
main = do
  putStrLn "How many groups?"
  size <- getLine
  putStrLn "Type one name per line."
  putStrLn "A group number will be assigned to the name."
  putStrLn "Ctrl-Z to produce a summary report"
  g <- getStdGen
  c <- getContents
  let names = lines c
      indices = (randomRs (1, read size) g  :: [Int]) 
  let entries = zipWith (\x y -> x ++ " " ++ y) (map show indices) names
  mapM_ putStrLn entries
  putStrLn "\nSummary:"      
  mapM_ putStrLn entries
  
  
