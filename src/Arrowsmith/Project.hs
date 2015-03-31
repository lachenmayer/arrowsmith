module Arrowsmith.Project where

import Control.Monad.Error (runErrorT)
import Data.Maybe (catMaybes)
import Elm.Package.Description
import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)

import Arrowsmith.ElmFile
import Arrowsmith.Types

getProject :: Repo -> IO (Either String Project)
getProject repo' = do
  d <- getDescription repo'
  sources' <- elmFiles repo'
  return $ case d of
    Left err -> Left $ "repo at " ++ (repoPath repo') ++ " is not a valid elm repo: " ++ err
    Right description' ->
      Right Project
        { Arrowsmith.Types.repo = repo'
        , description = description'
        , sources = sources'
        }

getDescription :: Repo -> IO (Either String Description)
getDescription repo' = do
  let packagePath = repoPath repo' </> "elm-package.json"
  runErrorT $ Elm.Package.Description.read packagePath

elmFiles :: Repo -> IO [ElmFile]
elmFiles repo' = do
  allFiles <- index repo'
  let filesToConvert = filter (\f -> takeExtension f == ".elm") allFiles
  elmFiles' <- mapM (elmFile repo') filesToConvert
  return $ catMaybes elmFiles'

--compile :: Project -> IO (Either String Project)
--compile project = do
--  -- TODO