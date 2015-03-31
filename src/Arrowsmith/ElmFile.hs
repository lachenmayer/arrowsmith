module Arrowsmith.ElmFile where

import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Arrowsmith.Types

elmFile :: Repo -> FilePath -> IO (Maybe ElmFile)
elmFile repo' fileName = do
  let fullPath = repoPath repo' </> fileName
  exists <- doesFileExist fullPath
  return $ if exists
    then
      Just ElmFile
        { filePath = fileName
        , compiled = False
        , modul = Nothing
        }
    else
      Nothing

--compile :: Repo -> ElmFile -> IO (CompileStatus, ElmFile)
--compile repo' elmFile' =


fullPath :: Repo -> ElmFile -> FilePath
fullPath repo' elmFile' =
  repoPath repo' </> filePath elmFile'