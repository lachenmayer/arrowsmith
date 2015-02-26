{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arrowsmith.Api (routes) where

import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.UTF8 as UTF8BS
import Snap.Core
import Snap.Extras.JSON (writeJSON)

import Arrowsmith.Compile (compile)


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
  [ ("compile", method POST compileHandler)
  ]

compileHandler :: Snap ()
compileHandler = do
  program <- readRequestBody 50000
  writeText "foo."
  --compiledProgram <- liftIO . runErrorT . compile $ LazyBS.toStrict program
  --case compiledProgram of
  --  Right (ast, code) ->
  --    writeJSON $ CompileSuccess ast code
  --  Left err -> do
  --    modifyResponse $ setResponseCode 400 -- Bad Request
  --    writeJSON $ CompileError err
  