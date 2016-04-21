-- works only for coprime pair
triangleArea' :: Int -> Int -> Float
triangleArea' a b
  | a == b = error "not coprime!"
  | a > b && a `mod` b == 0 = error "not coprime!"
  | a > b  = fromIntegral $sum $ map  ((`div` b) . (* a)) [1..b-1]
  | otherwise = triangleArea' b a

triangleArea a b = triangleArea' (a+1) (b+1)
