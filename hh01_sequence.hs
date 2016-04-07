sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (x:xs) = do
  x' <- x
  xs' <- (sequence' xs)
  return (x':xs')
