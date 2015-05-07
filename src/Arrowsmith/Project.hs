module Arrowsmith.Project where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (whenRight)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
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
      elmFiles' <- getElmFiles repoInfo' description'
      return $ Right Project
        { projectRepo = repoInfo'
        , elmFiles = HashMap.fromList $ map (\f -> (fileName f, f)) elmFiles'
        }

-- Creates a new project if not found.
getProject :: IORef ProjectsMap -> RepoInfo -> IO (Either String Project)
getProject projectsRef repoInfo' = do
  projects' <- readIORef projectsRef
  case HashMap.lookup repoInfo' projects' of
    Nothing -> do
      newProject <- createProject repoInfo'
      whenRight newProject (saveProject projectsRef)
      return newProject
    Just p -> return $ Right p

saveProject :: IORef ProjectsMap -> Project -> IO ()
saveProject projectsRef project' =
  atomicModifyIORef' projectsRef $ \ps ->
    (HashMap.insert (projectRepo project') project' ps, ())

getElmFile :: Project -> QualifiedName -> Maybe ElmFile
getElmFile project' name' =
  HashMap.lookup name' (elmFiles project')

-- saveElmFile :: IORef ProjectsMap -> ElmFile -> IO ()
-- saveElmFile projectsRef elmFile' = do
--   project' <- getProject projectsRef (inRepo elmFile')

updateElmFile :: (Applicative m, MonadIO m) => IORef ProjectsMap -> RepoInfo -> QualifiedName -> (Maybe ElmFile -> m EditResponse) -> m EditResponse
updateElmFile projectsRef repoInfo' fileName' updateFn = do
  project' <- liftIO $ getProject projectsRef repoInfo'
  case project' of
    Left _ -> return $ EditFailure "Couldn't get project."
    Right oldProject -> do
      let oldFile = getElmFile oldProject fileName'
      updated <- updateFn oldFile
      case updated of
        EditSuccess (CompileSuccess newFile) -> do
          let insertFile file = HashMap.insert (fileName file) file
              newProject = oldProject { elmFiles = insertFile newFile (elmFiles oldProject) }
          liftIO $ saveProject projectsRef newProject
          return updated
        _ -> return updated

getElmFiles :: RepoInfo -> Description -> IO [ElmFile]
getElmFiles repoInfo' description' = do
  repo <- getRepo repoInfo'
  case repo of
    Left _ -> return []
    Right repo' -> do
      allFiles <- index repo'
      let filesToConvert = filter (\f -> takeExtension f == ".elm") allFiles
      elmFiles' <- mapM (elmFile repoInfo' description') filesToConvert
      (return . catMaybes) elmFiles'
