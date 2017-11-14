{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Logger where

import Control.Exception (IOException, try)
import Control.Monad     (when)
import Yesod

data LoggerApp = App
instance Yesod LoggerApp where
  shouldLog App src level =
    True -- good for development
    -- level = LevelWarn || level == LevelError -- good for production

mkYesod "LoggerApp" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = do
  $logDebug "Trying to read data file"
  edata <- liftIO $ try $ readFile "datafile.txt"
  case edata :: Either IOException String of
    Left e -> do
        $logError $ "Could not read datafile.txt"
        defaultLayout [whamlet|An error occurred|]
    Right str -> do
      $logInfo "Reading data file succeeded"
      let ls = lines str
      when (length ls < 5) $ $logWarn "Less than 5 lines of data"
      defaultLayout
        [whamlet|
          <ol>
            $forall l <- ls
                <li>#{l}
              <span>cheese
              <p>ola1
            <span>ham
            <p>ola2
          <p>ola3
        |]

main :: IO ()
main = warp 3000 App