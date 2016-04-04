-- Problem: Num type class does not make == operator mandatory for its class memeber.
-- Solution: add Eq type class.
f :: (Eq a,Num a) => [a] -> a
f lst 
  | lst == [] = 0
  | otherwise = (head lst) + f(tail lst)

