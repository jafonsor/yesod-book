{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module ShakespearRoutesParams where

import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, append, pack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding (decodeUtf8)
import Blaze.ByteString.Builder (toByteString)

data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "/home" `append`
  decodeUtf8 (toByteString $ renderQueryText True (map (second Just) params))

-- printErrorMessage :: Bool -> String -> (MyRoute -> [t] -> Text) -> Text.Blaze.Internal.MarkupM ()
printErrorMessage isError errorMessage = [hamlet|
    <p #error_box :isError:.error .box :not isError:style="display: none;">
      #{errorMessage}
  |]

arbitrary =
  let
    attribs = [
        ("checked", "false"),
        ("onclick","console.log(\"click\")")
      ] :: [(Text, Text)]
  in
    [hamlet|
      <div *{attribs}>
          Arbitrary text.
    |]

printPrize maybePrize = [hamlet|
    $maybe prize <- maybePrize
      <div .prize>
        ^{prize}
  |]

bigPrize = [hamlet|
    <p>Congrats!! You are the luky winner of a great prize.
  |]


main :: IO ()
main = do
  let currPage = 2 :: Int
  let isError = True
  let errorMessage = "fill the tham form!!!" :: Text
  let prize = Just bigPrize
  putStrLn $ renderHtml $ [hamlet|
    <p>
      You are currently on page #{currPage}
      <a href=@?{(SomePage, [("page", pack $ show $ currPage - 1)])}>Prev
      <a href=@?{(SomePage, [("page", pack $ show $ currPage + 1)])}>Next
    <input type=checkbox checked>
    ^{printErrorMessage isError errorMessage}
    ^{arbitrary}
    ^{printPrize prize}
  |] render

