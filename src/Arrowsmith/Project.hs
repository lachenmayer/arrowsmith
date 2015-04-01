module Arrowsmith.Project where

import Control.Monad.Error (runErrorT)
import Data.Maybe (catMaybes)
import Elm.Package.Description
import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)

import Arrowsmith.ElmFile
import Arrowsmith.Repo
import Arrowsmith.Types

getProject :: Repo -> IO (Either String Project)
getProject repo' = do
  d <- getDescription repo'
  return $ case d of
    Left err -> Left $ "repo at " ++ (repoPath repo') ++ " is not a valid elm project: " ++ err
    Right description' -> do
      sources' <- elmFiles repo' description'
      return Project
        { Arrowsmith.Types.repo = repo'
        , description = description'
        , sources = sources'
        }

elmFiles :: Repo -> Description -> IO [ElmFile]
elmFiles repo' description' = do
  allFiles <- index repo'
  let filesToConvert = filter (\f -> takeExtension f == ".elm") allFiles
  elmFiles' <- mapM (elmFile repo' description') filesToConvert
  return $ catMaybes elmFiles'

--compile :: Project -> IO (Either String Project)
--compile project = do
--  -- TODO