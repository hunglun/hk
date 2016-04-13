import Control.Monad.Writer

logNum :: Int -> Writer [String] Int
logNum x = writer (x, ["Log:" ++ show x])

main = do
  interact $unlines . map ( head . snd . runWriter . logNum . read) . lines
