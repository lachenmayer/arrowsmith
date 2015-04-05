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
import Data.List.Utils (split)
import Data.Text (pack)
import Data.Text.Lazy (toStrict, unpack)
import Snap.Core
import Snap.Extras.JSON (writeJSON)
import Text.Blaze.Html (Html, (!), toMarkup)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), (<.>))
import System.IO.Error (tryIOError)

import Arrowsmith.ElmFile
import Arrowsmith.Module
import Arrowsmith.Repo
import Arrowsmith.Types
import Arrowsmith.Project


type Route = (BS.ByteString, Snap ())

routes :: [Route]
routes =
  [ (":backend/:user/:project/:module", method GET moduleHandler)
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

moduleHandler :: Snap ()
moduleHandler = do
  repoInfo <- getRepoInfo
  moduleName <- urlFragment "module"
  let fileName' = split "." moduleName
  project <- liftIO $ getProject repoInfo
  case project of
    Left err -> (writeText . pack) err
    Right project' -> do
      case fileWithName project' fileName' of
        Nothing -> writeText "module not found"
        Just elmFile' -> do
          latestFile <- liftIO $ getLatest elmFile'
          case latestFile of
            Left err -> (writeText . pack) err
            Right latestFile' -> do
              let moduleJson = (encode . toJSON) latestFile'
              let moduleScript = H.script ! A.class_ "initial-module" ! A.type_ "text/json" $ H.unsafeLazyByteString moduleJson
              (writeText . toStrict . renderHtml . editor) moduleScript

updateHandler :: Snap ()
updateHandler = do
  writeText "not implemented yet."

urlFragment :: BS.ByteString -> Snap String
urlFragment paramName = do
  param <- getParam paramName
  case param of
    Just value -> return $ UTF8BS.toString value
    Nothing -> error "URL handling is broken in Snap... (Arrowsmith.Api)"