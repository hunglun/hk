import System.Random

main = do
  putStrLn "Rolling 6 dies ... "
  g <- getStdGen
  let dies = take 6 $ randomRs (1,6) g  :: [Int]
  mapM_ (putStrLn . show ) dies
  
