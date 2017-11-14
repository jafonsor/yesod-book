{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Json where
import Yesod


data Json = Json

mkYesod "Json" [parseRoutes|
/json  JsonR  GET
|]

instance Yesod Json

getJsonR  = return $ object [ "msg" .= "Hello World" ]

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  warp port Json
