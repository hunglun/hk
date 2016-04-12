-- Textbook Algorithms
-- inspire problems to be solved in haskell.


mean :: Fractional a => [a] -> Maybe a
mean [] = Nothing
mean x = Just $ sum x / (fromIntegral . length) x

toNum :: String -> Float
toNum x = read x :: Float

main = do
  putStrLn "Computing the mean of the user defined numbers."
  -- reverse force the program to start computing mean after EOF
  interact  $ (  show . mean . map toNum . reverse . lines) 


-- next challenge:
--  dynamically compute mean
--  gracefully handle digit conversion error
