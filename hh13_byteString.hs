import System.Environment
import qualified Data.ByteString.Lazy as B

-- Measure-Command {C:\Users\ao1\code\hk\hh13_byteString.exe .emacs toDelete}
-- Performance boost with ByteString

-- TotalMilliseconds : 242.8882
-- TotalMilliseconds : 166.7938
main = do
  f1:f2:_ <- getArgs
  copyFile f1 f2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  c <- B.readFile source
  B.writeFile dest c
  
