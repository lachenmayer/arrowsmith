{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arrowsmith.Editor (routes) where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.UTF8 as UTF8BS
import Data.List.Utils (split)
import Data.Text (pack)
import Data.Text.Lazy (toStrict)
import Snap.Core
import Snap.Extras.JSON (getJSON, writeJSON)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Arrowsmith.ElmFile
import Arrowsmith.Types
import Arrowsmith.Project


routes :: [Route]
routes =
  [ (":backend/:user/:project/:module", method GET moduleHandler)
  , (":backend/:user/:project/:module/edit", method POST editHandler)
  ]

template :: Html -> Html
template contents =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Arrowsmith"
      H.link ! A.rel "stylesheet" ! A.href "/app/style.css"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.body $ do
      H.script ! A.src "/app/editor.js" $ return ()
      H.script ! A.src "/app/env.js" $ return ()
      contents

getReqRepoInfo :: AppHandler RepoInfo
getReqRepoInfo = do
  b <- urlFragment "backend"
  u <- urlFragment "user"
  p <- urlFragment "project"
  return RepoInfo { backend = b, user = u, project = p }

getReqModuleName :: AppHandler QualifiedName
getReqModuleName = do
  moduleName <- urlFragment "module"
  return $ split "." moduleName

getReqProject :: AppHandler (Either String Project)
getReqProject = do
  repoInfo' <- getReqRepoInfo
  projectsRef <- gets _projects
  liftIO $ getProject projectsRef repoInfo'

readElmFile :: AppHandler (Either String ElmFile)
readElmFile = do
  project' <- getReqProject
  moduleName <- getReqModuleName
  return $ case project' of
    Left err -> Left err
    Right p -> maybe (Left "elm file not found") Right $ getElmFile p moduleName

moduleHandler :: AppHandler ()
moduleHandler = do
  elmFile' <- readElmFile
  case elmFile' of
    Left err -> serverError err
    Right elmFile'' -> do
      compiled <- liftIO $ getLatest elmFile''
      case compiled of
        Left err -> (writeText . pack) err
        Right modul' -> do
          let moduleJson = (UTF8BS.toString . LazyBS.toStrict . encode . toJSON) modul'
              moduleScript = H.script ! A.class_ "initial-module" ! A.type_ "text/json" $ H.preEscapedString moduleJson
          (writeText . toStrict . renderHtml . template) moduleScript

editHandler :: AppHandler ()
editHandler = do
  repoInfo' <- getReqRepoInfo
  projectsRef <- gets _projects
  moduleName <- getReqModuleName
  action' <- getJSON
  case action' of
    Left err -> badRequest err
    Right action'' -> do
      editResponse <- liftIO $ updateElmFile projectsRef repoInfo' moduleName (performEdit action'')
      writeJSON editResponse

performEdit :: Action -> Maybe ElmFile -> IO EditResponse
performEdit action' maybeElmFile = do
  case maybeElmFile of
    Nothing -> return $ EditFailure "Elm file not found."
    Just elmFile' -> do
      compiled <- liftIO $ getLatest elmFile'
      case compiled of
        Left err -> return $ EditFailure err
        Right compiledFile -> do
          editResult <- liftIO $ edit compiledFile action'
          case editResult of
            Just editedFile -> do
              return $ EditSuccess (toCompileResponse (Right editedFile)) -- TODO
            Nothing -> return $ EditFailure "Probably couldn't compile the file."

urlFragment :: MonadSnap m => BS.ByteString -> m String
urlFragment paramName = do
  param <- getParam paramName
  case param of
    Just value -> return $ UTF8BS.toString value
    Nothing -> error "URL handling is broken in Snap... (Arrowsmith.Api)"

withResponseCode :: MonadSnap m => Int -> String -> m ()
withResponseCode code message = do
  modifyResponse $ setResponseCode code
  (writeText . pack) message

-- notFound :: MonadSnap m => String -> m ()
-- notFound message =
--   withResponseCode 404 message

badRequest :: MonadSnap m => String -> m ()
badRequest message =
  withResponseCode 400 message

serverError :: MonadSnap m => String -> m ()
serverError message =
  withResponseCode 500 message
