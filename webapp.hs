{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseFile, rawPathInfo, responseLBS)
import           Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Char8 as C
main :: IO ()
main = run 80 app

app :: Application
app req sendResponse =
  case rawPathInfo req of    
    x -> sendResponse $ responseFile
      status200
      [("Content-Type", "text/html")]
      (C.unpack x)
      Nothing
