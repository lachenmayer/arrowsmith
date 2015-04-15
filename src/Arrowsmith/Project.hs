module Arrowsmith.Project where

import Control.Monad (when)
import Data.Either (isRight)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List (find)
import Data.Maybe (catMaybes)
import System.FilePath.Posix (takeExtension)

import Elm.Package.Description (Description)

import Arrowsmith.ElmFile
import Arrowsmith.Paths
import Arrowsmith.Repo
import Arrowsmith.Types

createProject :: RepoInfo -> IO (Either String Project)
createProject repoInfo' = do
  d <- getDescription (repoPath repoInfo')
  case d of
    Left err ->
      return . Left $ "repo at " ++ repoPath repoInfo' ++ " is not a valid elm project: " ++ err
    Right description' -> do
      sources' <- elmFiles repoInfo' description'
      return $ Right Project
        { projectRepo = repoInfo'
        , sources = sources'
        }

-- Creates a new project if not found.
getProject :: IORef ProjectsMap -> RepoInfo -> IO (Either String Project)
getProject projectsRef repoInfo' = do
  projects' <- readIORef projectsRef
  case HashMap.lookup repoInfo' projects' of
    Nothing -> do
      newProject <- createProject repoInfo'
      when (isRight newProject) $ let Right p = newProject in saveProject projectsRef p
      return newProject
    Just p -> return $ Right p

saveProject :: IORef ProjectsMap -> Project -> IO ()
saveProject projectsRef project' =
  atomicModifyIORef' projectsRef $ \ps ->
    (HashMap.insert (projectRepo project') project' ps, ())

elmFiles :: RepoInfo -> Description -> IO [ElmFile]
elmFiles repoInfo' description' = do
  repo <- getRepo repoInfo'
  case repo of
    Left _ -> return []
    Right repo' -> do
      allFiles <- index repo'
      let filesToConvert = filter (\f -> takeExtension f == ".elm") allFiles
      elmFiles' <- mapM (elmFile repoInfo' description') filesToConvert
      return $ catMaybes elmFiles'

fileWithName :: Project -> QualifiedName -> Maybe ElmFile
fileWithName project' name' =
  find (\file -> fileName file == name') (sources project')

