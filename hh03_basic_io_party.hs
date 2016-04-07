-- putStr does not push out characters in window cmd.
main = do
  putStrLn "Username:"
  u <- getLine
  putStrLn "Password:"
  p <- getLine
  putStrLn (u ++ ":" ++ p ++ "@" ++ "http://apsgsgpgp8sf42/")
