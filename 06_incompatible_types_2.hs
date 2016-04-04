-- there is no easy way for different type to work together.

-- error
f :: (Num a, Num b, Num c)  => a -> b -> c
f x y = x*x + y*y
i :: Int
i = 3
j :: Float
j = 3.12


main = print (f  j i)
