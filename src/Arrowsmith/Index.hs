{-# LANGUAGE OverloadedStrings #-}
module Arrowsmith.Index where

import Data.Text.Lazy (toStrict)
import Snap.Core (writeText)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Arrowsmith.Types

indexTemplate :: Html
indexTemplate =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Arrowsmith"
      H.link ! A.rel "stylesheet" ! A.href "/app/style.css"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.body $
      H.a ! A.href "/github/lachenmayer/secret-sauce/Foo" $ "go to the example module."

indexRoute :: Route
indexRoute =
  ("/", (writeText . toStrict . renderHtml) indexTemplate)
