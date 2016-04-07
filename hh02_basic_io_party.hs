--
main = do
  c <- getChar
  if c == '.'
  then return ()
  else do
    putChar c
    main 
