import Data.Char
david :: String -> String
david x
    | x == "help" = "Help Menu" 
    | otherwise = "Unknown command"

main = do
  interact  (unlines . (map david) . lines) 
  
