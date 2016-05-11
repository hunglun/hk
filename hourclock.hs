import Test.QuickCheck
hours = [1..12]
--data Hours = 1|2|3|4|5|6|7|8|9|10|11|12
start :: Int -> Bool
start a = elem a hours

next :: Int -> Int
next hr = if hr /= 12 then hr + 1 else 1

invariant :: Int -> Bool
invariant = (\x-> if (x `elem` [1..12]) then start x && ((start . next) x) else True)

-- https://wiki.haskell.org/Introduction_to_QuickCheck1

