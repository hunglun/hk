-- USAGE
-- B:\jenkins\workspace\UT_SafetyL7xSJob\v\LogixTests\Safety\UnitTests>runghc -ic:\Users\ao1\code\hk c:\Users\ao1\code\hk\covReport.hs saPart.py _saPart_py

-- create coverage.xml
-- C:\snapshot\ice1_main_dev\LogixTests\Safety\UnitTests>xfind _saPart_py -name "*.coverage" |xargs -I zzz "C:\Users\ao1\code\cSharp\coverage2xml\bin\Release\coverage2xml.exe" zzz zzz.xml

import System.Process
import System.FilePath
import Data.List.Split
import Text.Regex.Posix
import System.Environment
import Data.List (nub,sort,intercalate)
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
       pattern2 = "{{([_:0-9a-zA-Z]+)}}"
       match2 = (s =~ pattern2 :: String)
       funcSymbolList = (['_','.'] ++ ['a'..'z'] ++ ['A'..'Z'] ++['0'..'9'])

main = do
  args <- getArgs
  let filePath = args !! 0
  let covDir = args !! 1
  c <- readFile filePath
    
  coverageFiles <- simpleFind "xml" covDir

  fList <- case (takeExtension filePath) of 
    ".py"  ->  return $(nub . sort . filter (/="") . map funcUnderTestPy  . lines) c
    ".aut" ->  return $(nub . sort . filter (/="") . map funcUnderTestAut . lines) c
    _ -> return []

  let commands = ["C:\\Users\\ao1\\code\\python\\coverageXmlParser.py " ++  covFile ++ " " ++ f |  f <-fList,covFile <- coverageFiles]
  mapM (\cmd -> do
        result <- readCreateProcess (shell cmd) ""
        let covFile = (!! 1) $ splitOn " " cmd
        let f = \res -> intercalate "$" ( res ++ [covFile])
        mapM putStrLn $(map f . chunksOf 6 . lines) result

       ) commands
