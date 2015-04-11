{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef)
import Snap.Http.Server
import Snap.Snaplet
import Snap.Util.FileServe (serveDirectoryWith, simpleDirectoryConfig)

import Arrowsmith.Editor as Editor
import Arrowsmith.Index (indexRoute)
import Arrowsmith.Types


staticRoutes :: [Route]
staticRoutes = map (\(url, fsPath) -> (url, serveDirectoryWith simpleDirectoryConfig fsPath))
  [ ("app", "frontend/bin")
  , ("public", "frontend/public")
  ]

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
