{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module DefaultLayout where

import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
--  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    pc <- widgetToPageContent $ do
            widget
            toWidget [lucius| body { font-family: verdana } |]
    withUrlRenderer
      [hamlet|
        $doctype 5
        <html>
          <head>
            <title>Example Page - #{pageTitle pc}
            <meta charset=utf-8>
            ^{pageHead pc}
          <body>
            <article>
              ^{pageBody pc}
      |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Home"
  [whamlet|
    <p>HelloWorld!
  |]


main :: IO ()
main = warp 3000 App