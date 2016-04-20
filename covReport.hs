-- USAGE
-- B:\jenkins\workspace\UT_SafetyL7xSJob\v\LogixTests\Safety\UnitTests>runghc -ic:\Users\ao1\code\hk c:\Users\ao1\code\hk\covReport.hs saPart.py

import System.FilePath
import Text.Regex.Posix
import System.Environment
import Data.List (nub,sort)
import StringUtils (strip)
import FindFiles (simpleFind)
-- python unit test 
funcUnderTestPy :: String -> String
funcUnderTestPy s = filter (\x ->notElem x ['{','}']) $ dropWhile (/='{') (s =~ pattern :: String)
                  where pattern = "Unit[ ]+testing[ ]+{{([_a-zA-Z]+)}}"
-- aut unit test
funcUnderTestAut :: String -> String
funcUnderTestAut s
    | match1 /= "" && '=' `elem` s = takeWhile (\x ->elem x funcSymbolList)
                              $strip $drop 1 $ dropWhile (/='=') match1
    | match1 /= "" && '=' `notElem` s = takeWhile (\x ->elem x funcSymbolList)
                                 $strip $drop 4 match1
    | match2 /= "" = filter (\x ->notElem x ['{','}']) $ dropWhile (/='{') match2
    | otherwise = ""
 where pattern1 = "exe .+"
       match1 = (s =~ pattern1 :: String)
       pattern2 = "{{([_a-zA-Z]+)}}"
       match2 = (s =~ pattern2 :: String)
       funcSymbolList = (['_','.'] ++ ['a'..'z'] ++ ['A'..'Z'] ++['0'..'9'])
main = do
  args <- getArgs
  let filePath = args !! 0
  c <- readFile filePath

  -- script -> [function name]
  case (takeExtension filePath) of 
    ".py"  ->  print $(nub . sort . filter (/="") . map funcUnderTestPy  . lines) c
    ".aut" ->  print $(nub . sort . filter (/="") . map funcUnderTestAut . lines) c
    _ -> print "Unknown test script type"
  -- TODO IoConnSafetySchb_001L000 could exist!
  let outputDir = takeDirectory filePath ++ "\\_runAllTests_py\\batch0\\" ++ (takeBaseName filePath) ++"_000L000"
  coverageFiles <- simpleFind "coverage" outputDir
  print coverageFiles
