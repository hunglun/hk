main = do
  d <- getLine
  appendFile "hello.txt" d
