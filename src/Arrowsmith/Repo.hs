module Arrowsmith.Repo where

import qualified Data.FileStore
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import Arrowsmith.Types

getRepo :: FilePath -> RepoInfo -> IO (Either String Repo)
getRepo reposPath repoInfo' = do
  let repoPath' = reposPath </> (backend repoInfo') </> (user repoInfo') </> (project repoInfo')
  exists <- doesDirectoryExist repoPath'
  return $ if exists
    then
      let fileStore = Data.FileStore.gitFileStore repoPath' in
        Right Repo
          { repoPath = repoPath'
          , repoInfo = repoInfo'
          , index = Data.FileStore.index fileStore
          , retrieve = Data.FileStore.retrieve fileStore
          }
    else
      Left "unimplemented: get it from github?"
