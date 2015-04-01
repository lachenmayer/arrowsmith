module Arrowsmith.Repo where

import Control.Monad.Error (runErrorT)
import qualified Data.FileStore
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as ElmPaths

import Arrowsmith.Paths (reposPath)
import Arrowsmith.Types

getRepo :: RepoInfo -> IO (Either String Repo)
getRepo repoInfo' = do
  let repoPath' = reposPath </> backend repoInfo' </> user repoInfo' </> project repoInfo'
  exists <- doesDirectoryExist repoPath'
  -- TODO check if git directory exists
  return $ if exists
    then
      let fileStore = Data.FileStore.gitFileStore repoPath' in
        Right Repo
          { repoPath = repoPath'
          , repoInfo = repoInfo'
          , index = Data.FileStore.index fileStore
          , latest = Data.FileStore.latest fileStore
          , retrieve = Data.FileStore.retrieve fileStore
          }
    else
      Left "unimplemented: get it from github?"
