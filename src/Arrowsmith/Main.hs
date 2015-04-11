{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar (newMVar)
import Control.Monad.IO.Class (liftIO)
import Snap.Http.Server
import Snap.Snaplet
import Snap.Util.FileServe (serveDirectoryWith, simpleDirectoryConfig)

import Arrowsmith.Api as Api
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
  addRoutes Api.routes

  projects' <- liftIO $ newMVar "foobarbaz"
  return $ App projects'

main :: IO ()
main =
  serveSnaplet defaultConfig appInit