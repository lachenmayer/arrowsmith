{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Text.Lazy (toStrict)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe (serveDirectoryWith, simpleDirectoryConfig)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Arrowsmith.Api as Api

type Route = (BS.ByteString, Snap ())

staticRoutes :: [Route]
staticRoutes = map (\(url, fsPath) -> (url, serveDirectoryWith simpleDirectoryConfig fsPath))
  [ ("app", "frontend/bin")
  , ("public", "frontend/public")
  ]

index :: Html
index =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Arrowsmith"
      H.link ! A.rel "stylesheet" ! A.href "/app/style.css"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.body $ do
      H.a ! A.href "/github/lachenmayer/secret-sauce/Foo" $ "go to the example module."

app :: Snap ()
app =
  ifTop ((writeText . toStrict . renderHtml) index) <|>
  route staticRoutes <|>
  route Api.routes

main :: IO ()
main =
  quickHttpServe app