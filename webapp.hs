{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod

data MyWebSite = MyWebSite

instance Yesod MyWebSite

mkYesod "MyWebSite" [parseRoutes|
  / HomeR GET
|]

-- show
getHomeR = defaultLayout $ do
  [whamlet|
    <h2> Things To Do
    <ul>
      <li> Learn Haskell
      <li> Write a killer app
      <li> Create a startup
      <li> Go public
  |]
  toWidget [cassius|
    body
      background-color: #edf
  |]
-- /show
-- how to set PORT in environment?
-- main = warpEnv MyWebSite
main = warpDebug 3000 MyWebSite 
