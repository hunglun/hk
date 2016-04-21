-- USAGE
-- B:\jenkins\workspace\UT_SafetyL7xSJob\v\LogixTests\Safety\UnitTests>runghc -ic:\Users\ao1\code\hk c:\Users\ao1\code\hk\covReport.hs saPart.py _saPart_py

-- create coverage.xml
-- C:\snapshot\ice1_main_dev\LogixTests\Safety\UnitTests>xfind _saPart_py -name "*.coverage" |xargs -I zzz "C:\Users\ao1\Documents\Visual Studio 2010\Projects\ConsoleApplication1\ConsoleApplication1\bin\Debug\ConsoleApplication1.exe" zzz zzz.xml
import System.Process
import System.FilePath
import Data.List.Split
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
  let covDir = args !! 1
  c <- readFile filePath
    
  coverageFiles <- simpleFind "xml" covDir

  fList <- case (takeExtension filePath) of 
    ".py"  ->  return $(nub . sort . filter (/="") . map funcUnderTestPy  . lines) c
    ".aut" ->  return $(nub . sort . filter (/="") . map funcUnderTestAut . lines) c
    _ -> return []

  let commands = ["c:\\Users\\ao1\\Documents\\work\\coverageXmlParser\\t.py " ++  covFile ++ " " ++ f | covFile <- coverageFiles, f <-fList]
  mapM (\cmd -> do
        result <- readCreateProcess (shell cmd) ""
        if result /= ""
        then print ((reverse . drop 1 . splitOn " ") cmd ++ (drop 1 . lines) result)
        else putStr ""
       ) commands
