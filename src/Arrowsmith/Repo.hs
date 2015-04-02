module Arrowsmith.Repo where

import qualified Data.FileStore
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as ElmPaths

import Arrowsmith.Paths
import Arrowsmith.Types

getRepo :: RepoInfo -> IO (Either String Repo)
getRepo repoInfo' = do
  let repoPath' = repoPath repoInfo'
  exists <- doesDirectoryExist repoPath'
  -- TODO check if git directory exists
  return $ if exists
    then
      let fileStore = Data.FileStore.gitFileStore repoPath' in
        Right Repo
          { repoInfo = repoInfo'
          , index = Data.FileStore.index fileStore
          , latest = Data.FileStore.latest fileStore
          , retrieve = Data.FileStore.retrieve fileStore
          }
    else
      Left "unimplemented: get it from github?"
