f :: Num a => [a] -> a
f [] = 0
f (x:xs) = x + (f xs)
