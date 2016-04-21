import System.Process
freeByte :: String -> Integer
freeByte  = read . dropWhile (\x -> notElem x ['0'..'9']) 

-- if free bytes fall below 50 GB, return FALSE
storageCapacityTooLow :: Integer -> Bool
storageCapacityTooLow n = n < 50 * 1000 * 1000 * 1000

-- usage
-- c:\Users\ao1\code\hk>fsutil volume diskfree b: | runghc diskMon.hs
main = do
  -- first line describes the free bytes
  -- c:/Users/ao1/bcserver>fsutil volume diskfree b:
  -- Total # of free bytes        : 202043080704
  line <- getLine
  let freeBytes = freeByte line
  let emailBody = "\"B Drive free storage capacity too low: " ++ show freeBytes ++ "\""
  print freeBytes

  if (storageCapacityTooLow . freeByte) line
  then createProcess (shell $"c:/Python27/Lib/site-packages/rac/ramail.py ao1 " ++ emailBody )
  else createProcess (shell "rem" )
