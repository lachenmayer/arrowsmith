{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arrowsmith.Api (routes) where

import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.UTF8 as UTF8BS
import Data.Text.Lazy (toStrict)
import Snap.Core
import Snap.Extras.JSON (writeJSON)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Arrowsmith.Compile (compile)
import qualified Arrowsmith.Module as Module
import Arrowsmith.Repo


type Route = (BS.ByteString, Snap ())

data CompileResponse
  = CompileSuccess BS.ByteString BS.ByteString
  | CompileError String

instance ToJSON CompileResponse where
  toJSON (CompileSuccess ast code) =
    object ["ast" .= UTF8BS.toString ast, "code" .= UTF8BS.toString code]
  toJSON (CompileError message) =
    object ["error" .= message]


routes :: [Route]
routes =
  [ (":backend/:user/:project/:module", method GET editorHandler)
  , (":backend/:user/:project/:module/update", method POST updateHandler)
  , (":backend/:user/:project/:module/compile", method POST compileHandler)
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

getRepo :: Snap Repo
getRepo = do
  b <- urlFragment "backend"
  u <- urlFragment "user"
  p <- urlFragment "project"
  return Repo { backend = b, user = u, project = p }

editorHandler :: Snap ()
editorHandler = do
  repo <- getRepo
  modul <- urlFragment "module"
  m <- liftIO $ Module.getModule repo modul
  let moduleJson = UTF8BS.toString . LazyBS.toStrict . encode . toJSON $ m
  let moduleScript = H.p $ toMarkup moduleJson
  writeText . toStrict . renderHtml $ editor moduleScript

updateHandler :: Snap ()
updateHandler = do
  writeText "not implemented yet."

compileHandler :: Snap ()
compileHandler = do
  writeText "not implemented yet."
  --program <- readRequestBody 50000
  --compiledProgram <- liftIO . runErrorT . compile $ LazyBS.toStrict program
  --case compiledProgram of
  --  Right (ast, code) ->
  --    writeJSON $ CompileSuccess ast code
  --  Left err -> do
  --    modifyResponse $ setResponseCode 400 -- Bad Request
  --    writeJSON $ CompileError err

urlFragment :: BS.ByteString -> Snap String
urlFragment name = do
  param <- getParam name
  case param of
    Just value -> return $ UTF8BS.toString value
    Nothing -> error "URL handling is broken in Snap... (Arrowsmith.Api)"