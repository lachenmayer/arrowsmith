{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef)
import Data.Text.Lazy (toStrict)
import Snap.Core (writeText)
import Snap.Http.Server
import Snap.Snaplet
import Snap.Util.FileServe (serveDirectoryWith, simpleDirectoryConfig)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Arrowsmith.Editor as Editor
import Arrowsmith.Types


staticRoutes :: [Route]
staticRoutes = map (\(url, fsPath) -> (url, serveDirectoryWith simpleDirectoryConfig fsPath))
  [ ("app", "frontend/bin")
  , ("public", "frontend/public")
  ]

indexTemplate :: Html
indexTemplate =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Arrowsmith"
      H.link ! A.rel "stylesheet" ! A.href "/app/style.css"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.body $ do
      H.script ! A.src "/app/editor.js" $ return ()
      H.script ! A.src "/app/env.js" $ return ()

indexRoute :: Route
indexRoute =
  ("/", (writeText . toStrict . renderHtml) indexTemplate)

appInit :: SnapletInit App App
appInit = makeSnaplet "arrowsmith" "Arrowsmith" Nothing $ do
  addRoutes [indexRoute]
  addRoutes staticRoutes
  addRoutes Editor.routes

  projects' <- liftIO $ newIORef HashMap.empty
  return $ App projects'

main :: IO ()
main =
  serveSnaplet defaultConfig appInit
