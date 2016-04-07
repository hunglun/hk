import System.Environment
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn progName
  mapM putStrLn args
