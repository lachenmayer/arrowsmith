{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arrowsmith.Api (routes) where

import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.UTF8 as UTF8BS
import Data.Text.Lazy (toStrict)
import Snap.Core
import Snap.Extras.JSON (writeJSON)
import Text.Blaze.Html (Html, (!), toMarkup)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), (<.>))
import System.IO.Error (tryIOError)

import Arrowsmith.Module
import Arrowsmith.Repo
import Arrowsmith.Types
import Arrowsmith.Project


type Route = (BS.ByteString, Snap ())

routes :: [Route]
routes =
  [ (":backend/:user/:project/:module", method GET editorHandler)
  , (":backend/:user/:project/:module/update", method POST updateHandler)
  ]

editor :: Html -> Html
editor contents =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Arrowsmith"
      H.link ! A.rel "stylesheet" ! A.href "/app/style.css"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.body $ do
      H.script ! A.src "/app/editor.js" $ return ()
      H.script ! A.src "/app/env.js" $ return ()
      contents

getRepoInfo :: Snap RepoInfo
getRepoInfo = do
  b <- urlFragment "backend"
  u <- urlFragment "user"
  p <- urlFragment "project"
  return RepoInfo { backend = b, user = u, project = p }

editorHandler :: Snap ()
editorHandler = do
  --repo <- getRepo
  --modul <- urlFragment "module"
  --compiledProgram <- runCompile repo modul -- TODO properly: probably shouldn't have to recompile every time.
  --m <- liftIO $ getModule repo modul
  --let moduleJson = UTF8BS.toString . LazyBS.toStrict . encode . toJSON $ m
  --let moduleScript = H.script ! A.class_ "initial-program" ! A.type_ "text/json" $ toMarkup moduleJson
  --writeText . toStrict . renderHtml $ editor moduleScript
  writeText "not implemented yet."

updateHandler :: Snap ()
updateHandler = do
  writeText "not implemented yet."

urlFragment :: BS.ByteString -> Snap String
urlFragment paramName = do
  param <- getParam paramName
  case param of
    Just value -> return $ UTF8BS.toString value
    Nothing -> error "URL handling is broken in Snap... (Arrowsmith.Api)"