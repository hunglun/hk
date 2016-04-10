main = do
  putStrLn "Type something ... "
  r <- fmap (\x -> "> " ++ x) getLine
  putStrLn r

  putStrLn "Law 1 of Functor: fmap id = id"

  if (fmap id [1,2,3] == id [1,2,3])
    then putStrLn "Law 1 holds"
    else putStrLn "Law 1 is violated"

  putStrLn "Law 2 of Functor: fmap (f . g) = fmap f . fmap g"
  
  if (fmap ((+1) . (+2))  [1,2,3]) == (fmap (+1) . fmap (+2) )[1,2,3]
    then putStrLn "Law 2 holds"
    else putStrLn "Law 2 is violated"

