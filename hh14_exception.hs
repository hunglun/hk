import System.IO.Error
import System.Environment
-- import System.IO
-- replaced with Control.Exception
import Control.Exception
main = toTry `catch` handler


-- *Main> :info IOError
-- type IOError = IOException      -- Defined in `GHC.IO.Exception'
toTry :: IO ()
toTry = do
  filename:_ <- getArgs
  c <- (readFile filename)
  putStrLn c

handler :: IOError -> IO ()
handler e 
    | isDoesNotExistError e = putStrLn "File does not exist"
    | otherwise = putStrLn "some problem"
            
