{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arrowsmith.Editor (routes) where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8BS
import Data.List.Utils (split)
import Snap.Core
import Snap.Extras.JSON (getJSON, writeJSON)

import Arrowsmith.ElmFile
import Arrowsmith.Types
import Arrowsmith.Project


routes :: [Route]
routes =
  [ ("api/:backend/:user/:project", ifTop $ method GET projectHandler)
  , ("api/:backend/:user/:project/:module", ifTop $ method GET moduleHandler)
  , ("api/:backend/:user/:project/:module", ifTop $ method POST editHandler)
  ]

getReqRepoInfo :: AppHandler RepoInfo
getReqRepoInfo = do
  b <- urlFragment "backend"
  u <- urlFragment "user"
  p <- urlFragment "project"
  return RepoInfo { backend = b, user = u, project = p }

getReqModuleName :: AppHandler ModuleName
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

projectHandler :: AppHandler ()
projectHandler = do
  project' <- getReqProject
  case project' of
    Left err -> notFound err
    Right p -> do
      writeJSON p

moduleHandler :: AppHandler ()
moduleHandler = do
  elmFile' <- readElmFile
  case elmFile' of
    Left err -> serverError err
    Right elmFile'' -> do
      compiled <- liftIO $ getLatest elmFile''
      writeJSON compiled

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

performEdit :: EditAction -> Maybe ElmFile -> IO EditResponse
performEdit action' maybeElmFile = do
  case maybeElmFile of
    Nothing -> return $ EditFailure "Elm file not found."
    Just elmFile' -> do
      latestFile <- liftIO $ getLatest elmFile'
      editResult <- liftIO $ edit latestFile action'
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

errorObject :: String -> Value
errorObject message =
  object ["error" .= message]

withResponseCode :: MonadSnap m => Int -> String -> m ()
withResponseCode code message = do
  modifyResponse $ setResponseCode code
  (writeJSON . errorObject) message

notFound :: MonadSnap m => String -> m ()
notFound message =
  withResponseCode 404 message

badRequest :: MonadSnap m => String -> m ()
badRequest message =
  withResponseCode 400 message

serverError :: MonadSnap m => String -> m ()
serverError message =
  withResponseCode 500 message
