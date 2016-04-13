--  dynamically compute mean

-- Haskell is smart enough to infer that mean is used in two different senses
-- in this function.
mean :: Fractional a => (a,a) -> a -> (a,a)
mean (mean, count) input  = (((mean * count + input) / newCount), newCount)
  where newCount = count + 1

toNum :: (Fractional a, Read a) => String -> a
toNum x = read x 

main = do
  putStrLn "Computing the mean of the user defined numbers."
  interact  $ (  unlines .  map (("> "++)  . show . fst) . scanl mean (0,0)  . map toNum . lines)



-- next challenge:
-- add error handling in toNum
