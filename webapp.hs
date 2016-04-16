{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (Builder, fromByteString)
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseBuilder)
import           Network.Wai.Handler.Warp (run)

-- http://www.yesodweb.com/book/yesod-for-haskellers
-- Building Html Response    
main :: IO ()
main = run 3000 app
app :: Application

app _req sendResponse = sendResponse $ responseBuilder
    status200
    [("Content-Type", "text/plain")]
    (fromByteString "Hello from blaze-builder!" :: Builder)

