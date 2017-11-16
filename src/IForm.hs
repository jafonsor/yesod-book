{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module IForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text           (Text)
import Yesod

data IForm = IForm

mkYesod "IForm" [parseRoutes|
/ HomeR GET
/input InputR GET
|]

instance Yesod IForm

instance RenderMessage IForm FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person
    { personName :: Text
    , psersonAge :: Int
    }
    deriving Show

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
      <form action=@{InputR}>
        <p>
            My name is
            <input type=text name=name>
            and I am
            <input type=text name=age>
            years old.
            <input type=submit value="Introduce myself">
    |]

getInputR :: Handler Html
getInputR = do
  person <- runInputGet $ Person
                  <$> ireq textField "name"
                  <*> ireq intField "age"
  defaultLayout [whamlet|<p>#{show person}|]

main :: IO ()
main = warp 3000 IForm