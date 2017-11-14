{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Form where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Time --(Day)
import Yesod
import Yesod.Form.Jquery

data App = App


mkYesod "App" [parseRoutes|
/ HomeR GET
/person PersonR POST
/car    CarR    POST
|]

instance Yesod App

-- Tells our application to use standar English messages.
-- If you want i18n, then you can supply a translating function
-- instead.

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodJquery App

data Person = Person
  { personName         :: Text
  , personBirthday     :: Day
  , personFavoritColor :: Maybe Text
  , personEmail        :: Text
  , personWebsite      :: Maybe Text
  }
  deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
  <$> areq textField "Name" Nothing
  <*> areq (jqueryDayField def
      { jdsChangeYear = True --give a year dropdown
      , jdsYearRange = "1900:-5" -- 1900 till five years ago
      }) "Birthday" Nothing
  <*> aopt textField "Favorite color" Nothing
  <*> areq emailField "Email address" Nothing
  <*> aopt urlField "Website" Nothing

getHomeR :: Handler Html
getHomeR = do
  (formWidgetP, enctypeP) <- generateFormPost personForm
  (formWidgetC, enctypeC) <- generateFormPost carForm
  defaultLayout
    [whamlet|
      <p>
        The widget generated contains only the contents
        of the form, not the form tag itself. So...
        <form method=post action=@{PersonR} enctype=#{enctypeP}>
          ^{formWidgetP}
          <p>It also doesn't include the submit button.
          <button>Submit
      <p>
        Car form:
        <form method=post action=@{CarR} enctype=#{enctypeC}>
          ^{formWidgetC}
          <button>Submit
    |]

postPersonR :: Handler Html
postPersonR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
    FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form method=post action=@{PersonR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
      |]

postCarR :: Handler Html
postCarR = do
  ((result, widget), enctype) <- runFormPost carForm
  case result of
    FormSuccess car -> defaultLayout [whamlet|<p>#{show car}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form method=post action=@{CarR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
      |]


-- car example
data Car = Car
  { carModel :: Text
  , carYear  :: Int
  , carColor :: Maybe Color
  }
  deriving Show

data Color = Red | Blue | Gray | Black
    deriving (Show, Eq, Enum, Bounded)

carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField    "Model" (carModel <$> mcar)
    <*> areq carYearField "Year"  (carYear  <$> mcar)
    <*> aopt (selectFieldList colors) "Color" (carColor <$> mcar)
  where
    carTooOldMsg :: Text
    carTooOldMsg = "Your car is too old, get a new one!"

    -- faster way
    after1990Field = checkBool (>= 1990) carTooOldMsg intField

    {-
    -- general way 
    after1990 = check validateYear intField

    validateYear y
      | y < 1990  = Left carTooOldMsg
      | otherwise = Right y
    -}
    
    carYearField = checkM inPast after1990Field

    inPast y = do
      thisYear <- liftIO getCurrentYear
      return $ if y <= thisYear
          then Right y
          else Left  ("You have a time machine!" :: Text)

    colors :: [(Text, Color)]
    colors = [("Red", Red), ("Blue", Blue), ("Gray", Gray), ("Black", Black)]

getCurrentYear :: IO Int
getCurrentYear = do
  now <- getCurrentTime
  let today = utctDay now
  let (year, _, _) = toGregorian today
  return $ fromInteger year

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable $ carAForm $ Just $ Car "Forte" 2010 $ Just Gray



main :: IO ()
main = warp 3000 App