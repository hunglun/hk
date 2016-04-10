import Control.Applicative

main = do
  ( putStrLn . show ) $ (*) <$> [1,2,3] <*> [4,5,6]
  ( putStrLn . show ) $ (*) <$> Just 11 <*> Just 2
