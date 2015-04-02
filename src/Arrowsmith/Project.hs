module Arrowsmith.Project where

import Control.Monad.Error (runErrorT)
import Data.Maybe (catMaybes)
import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)

import Elm.Package.Description

import Arrowsmith.ElmFile
import Arrowsmith.Paths
import Arrowsmith.Repo
import Arrowsmith.Types

getProject :: RepoInfo -> IO (Either String Project)
getProject repoInfo' = do
  d <- getDescription (repoPath repoInfo')
  case d of
    Left err ->
      return . Left $ "repo at " ++ (repoPath repoInfo') ++ " is not a valid elm project: " ++ err
    Right description' -> do
      sources' <- elmFiles repoInfo' description'
      return $ Right Project
        { projectRepo = repoInfo'
        , description = description'
        , sources = sources'
        }

elmFiles :: RepoInfo -> Description -> IO [ElmFile]
elmFiles repoInfo' description' = do
  repo <- getRepo repoInfo'
  case repo of
    Left err -> return []
    Right repo' -> do
      allFiles <- index repo'
      let filesToConvert = filter (\f -> takeExtension f == ".elm") allFiles
      elmFiles' <- mapM (elmFile repoInfo' description') filesToConvert
      return $ catMaybes elmFiles'

--compile :: Project -> IO (Either String Project)
--compile project = do
--  -- TODO