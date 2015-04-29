module Arrowsmith.Repo where

import Control.Monad (liftM)
import qualified Data.FileStore
import System.Directory (doesDirectoryExist)

import qualified Arrowsmith.Git
import Arrowsmith.Paths
import Arrowsmith.Types


getRepo :: RepoInfo -> IO (Either String Repo)
getRepo repoInfo' = do
  let repoPath' = repoPath repoInfo'
  exists <- doesDirectoryExist repoPath'
  -- TODO check if git directory exists
  return $ if exists
    then
      let
        fileStore = Data.FileStore.gitFileStore repoPath'
      in
        Right Repo
          { repoInfo = repoInfo'
          , index = Data.FileStore.index fileStore
          , latest = Data.FileStore.latest fileStore
          , retrieve = Data.FileStore.retrieve fileStore
          , save = \p -> Data.FileStore.save fileStore p author
          , checkout = Arrowsmith.Git.gitCheckout repoPath'
          , repoHead = Arrowsmith.Git.gitHead repoPath'
          , repoBranch = Arrowsmith.Git.gitBranchName repoPath'
          , commitMessage = \rev -> liftM Data.FileStore.revDescription (Data.FileStore.revision fileStore rev)
          }
    else
      Left "unimplemented: get it from github?"
  where
    author = Data.FileStore.Author "Arrowsmith" "arrowsmith@no.email"
