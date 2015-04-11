{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arrowsmith.Api (routes) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.UTF8 as UTF8BS
import Data.List.Utils (split)
import Data.Text (pack)
import Data.Text.Lazy (toStrict)
import Snap.Core
import Snap.Extras.JSON (getJSON)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Arrowsmith.Edit
import Arrowsmith.ElmFile
--import Arrowsmith.Module
--import Arrowsmith.Repo
import Arrowsmith.Types
import Arrowsmith.Project

routes :: [Route]
routes =
  [ (":backend/:user/:project/:module", method GET moduleHandler)
  , (":backend/:user/:project/:module/edit", method POST editHandler)
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

getRepoInfo :: AppHandler RepoInfo
getRepoInfo = do
  b <- urlFragment "backend"
  u <- urlFragment "user"
  p <- urlFragment "project"
  return RepoInfo { backend = b, user = u, project = p }

getProject :: AppHandler (Either String Project)
getProject = do
  repoInfo' <- getRepoInfo
  -- TODO don't recreate project every time.
  liftIO $ createProject repoInfo'

getElmFile :: AppHandler (Maybe ElmFile)
getElmFile = do
  project' <- getProject
  moduleName <- urlFragment "module"
  let fileName' = split "." moduleName
  return $ case project' of
    Left _ -> Nothing
    Right project'' -> fileWithName project'' fileName'

moduleHandler :: AppHandler ()
moduleHandler = do
  elmFile' <- getElmFile
  case elmFile' of
    Nothing -> writeText "404 :("
    Just elmFile'' -> do
      compiled <- liftIO $ compile elmFile''
      case compiled of
        Left err -> (writeText . pack) err
        Right compiledFile' -> do
          let moduleJson = (UTF8BS.toString . LazyBS.toStrict . encode . toJSON) compiledFile'
          let moduleScript = H.script ! A.class_ "initial-module" ! A.type_ "text/json" $ H.preEscapedString moduleJson
          (writeText . toStrict . renderHtml . editor) moduleScript

editHandler :: AppHandler ()
editHandler = do
  elmFile' <- getElmFile
  case elmFile' of
    Nothing -> writeText "404 :/"
    Just elmFile'' -> do
      action' <- getJSON
      case action' of
        Left err -> (writeText . pack) err
        Right action'' -> do
          editResult <- liftIO $ performEdit elmFile'' action''
          case editResult of
            Nothing -> writeText "didn't edit."
            Just _ -> writeText "did actually edit."

urlFragment :: MonadSnap m => BS.ByteString -> m String
urlFragment paramName = do
  param <- getParam paramName
  case param of
    Just value -> return $ UTF8BS.toString value
    Nothing -> error "URL handling is broken in Snap... (Arrowsmith.Api)"