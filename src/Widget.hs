{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Widget where

import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR = defaultLayout $ do
  setTitle "My Page Title"
  toWidget [lucius| h1 {color: green; } |]
  addScriptRemote "http://code.jquery.com/jquery-1.9.1.js"
  toWidget
    [julius|
      $(function() {
        $("h1").click(function(){
          alert("You clicked on the heading!");
        });
      });
    |]
  toWidgetHead
    [hamlet|
      <meta name=keywords content="some sample keywords">
    |]
  toWidget
    [hamlet|
      <h1>Here's one way of including content
    |]
  [whamlet|<h2>Here's another |]
  toWidgetBody
    [julius|
      alert("This is included in the body itself");
    |]

main = warp 3000 App