module Arrowsmith.Project where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)
import System.FilePath.Posix (takeExtension)

import Elm.Package.Description (Description)

import Arrowsmith.ElmFile
import Arrowsmith.Paths
import Arrowsmith.Repo
import Arrowsmith.Types

getProject :: RepoInfo -> IO (Either String Project)
getProject repoInfo' = do
  repo <- getRepo repoInfo' -- will clone the repo if it doesn't exist
  case repo of
    Left err ->
      return . Left $ "repo at " ++ repoPath repoInfo' ++ " is not a valid elm project: " ++ err
    Right _ -> do
      d <- getDescription (repoPath repoInfo')
      case d of
        Left err ->
          return . Left $ "repo at " ++ repoPath repoInfo' ++ " is not a valid elm project: " ++ err
        Right description' -> do
          elmFiles' <- getElmFiles repoInfo' description'
          return $ Right Project
            { projectRepo = repoInfo'
            , elmFiles = HashMap.fromList $ map (\f -> ((nameToString . fileName) f, f)) elmFiles'
            }

getElmFile :: Project -> ModuleName -> Maybe ElmFile
getElmFile project' name' =
  HashMap.lookup (nameToString name') (elmFiles project')

updateElmFile :: (Applicative m, MonadIO m) => RepoInfo -> ModuleName -> (Maybe ElmFile -> m EditResponse) -> m EditResponse
updateElmFile repoInfo' fileName' updateFn = do
  project' <- liftIO $ getProject repoInfo'
  case project' of
    Left _ -> return $ EditFailure "Couldn't get project."
    Right p -> updateFn $ getElmFile p fileName'

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
