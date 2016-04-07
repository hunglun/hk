import Data.Char
main = do
  c <- getContents
  putStr $ (unlines . (map (\x -> reverse x)) . lines) c
  
