{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module MForm where

import Control.Applicative
import Data.Text (Text)
import Yesod

data MFormApp = MFormApp

mkYesod "MFormApp" [parseRoutes|
/ HomeR GET
|]

instance Yesod MFormApp

instance RenderMessage MFormApp FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person
  { psersonName :: Text
  , psersonAge  :: Int
  }
  deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm otherHtml = do
  (nameRes, nameView) <- mreq textField "this is not used" Nothing
  (ageRes,  ageView)  <- mreq intField  "neither is this"  Nothing
  let personRes = Person <$> nameRes <*> ageRes
  let widget = do
                toWidget
                  [lucius|
                    ##{fvId ageView} {
                        width: 3em;
                    }
                  |]
                [whamlet|
                  #{otherHtml}
                  <p>
                    Hello, my name is #
                    ^{fvInput nameView}
                    \ and I am #
                    ^{fvInput ageView}
                    \ years old. #
                    <input type=submit value="Introduce myself">
                |]
  return (personRes, widget)

getHomeR :: Handler Html
getHomeR = do
  ((res, widget), enctype) <- runFormGet personForm
  defaultLayout
    [whamlet|
        <p>Result: #{show res}
        <form enctype=#{enctype}>
          ^{widget}
    |]

main :: IO ()
main = warp 3000 MFormApp