{-# LANGUAGE OverloadedStrings #-}
module Arrowsmith.Templates where

import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

skeleton :: Html -> Html
skeleton body =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Arrowsmith"
      H.link ! A.rel "stylesheet" ! A.href "/app/style.css"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.body body

jsonScript :: H.AttributeValue -> String -> Html
jsonScript className contents =
 H.script ! A.class_ className ! A.type_ "text/json" $ H.preEscapedString contents

indexTemplate :: Html
indexTemplate =
   skeleton $
      H.a ! A.href "/github/lachenmayer/secret-sauce/Foo" $ "go to the example module."

moduleTemplate :: Html -> Html
moduleTemplate contents =
  skeleton $ do
      H.script ! A.src "/app/editor.js" $ return ()
      H.script ! A.src "/app/env.js" $ return ()
      contents

projectTemplate :: Html -> Html
projectTemplate contents =
  skeleton contents