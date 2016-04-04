f :: Num a  => a -> a -> a
f x y = x*x + y*y
i :: Int
i = 3
j :: Float
j = 3.12

main = print (f  j i)
