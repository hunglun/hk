textBox :: IO ()
textBox = do
  e <- getLine
  if e == "exit"
  then return ()
  else textBox

main = do
  putStrLn "Type exit to quit"
  textBox
