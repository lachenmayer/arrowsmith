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
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), (<.>))

import Arrowsmith.Compile (compile)
import Arrowsmith.Module
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

getModule :: Repo -> String -> IO (Maybe Module)
getModule repo modul = do
  basePath <- getCurrentDirectory
  astFile <- LazyBS.readFile $ (repoPath basePath repo) </> "elm-stuff/build-artifacts/USER/PROJECT/1.0.0" </> modul <.> "elma" -- TODO properly
  return $ (fromAstFile astFile) >>= Just . makeModule

runCompile :: (MonadIO m) => Repo -> String -> m (Either String (UTF8BS.ByteString, UTF8BS.ByteString))
runCompile repo modul =
  liftIO . runErrorT $ compile repo modul

editorHandler :: Snap ()
editorHandler = do
  repo <- getRepo
  modul <- urlFragment "module"
  compiledProgram <- runCompile repo modul -- TODO properly: probably shouldn't have to recompile every time.
  m <- liftIO $ getModule repo modul
  let moduleJson = UTF8BS.toString . LazyBS.toStrict . encode . toJSON $ m
  let moduleScript = H.script ! A.class_ "initial-program" ! A.type_ "text/json" $ toMarkup moduleJson
  writeText . toStrict . renderHtml $ editor moduleScript

updateHandler :: Snap ()
updateHandler = do
  writeText "not implemented yet."

compileHandler :: Snap ()
compileHandler = do
  repo <- getRepo
  modul <- urlFragment "module"
  compiledProgram <- runCompile repo modul
  case compiledProgram of
    Right (ast, code) ->
      writeJSON $ CompileSuccess ast code
    Left err -> do
      modifyResponse $ setResponseCode 400 -- Bad Request
      writeJSON $ CompileError err

urlFragment :: BS.ByteString -> Snap String
urlFragment name = do
  param <- getParam name
  case param of
    Just value -> return $ UTF8BS.toString value
    Nothing -> error "URL handling is broken in Snap... (Arrowsmith.Api)"