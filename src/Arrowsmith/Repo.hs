module Arrowsmith.Repo where

import Control.Monad (liftM)
import qualified Data.FileStore
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode)

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
      Right Repo { repoInfo = repoInfo' }
    else
      Left "unimplemented: get it from github?"

index :: Repo -> IO [FilePath]
index repo' =
  Data.FileStore.index (gitFileStore repo')

latest :: Repo -> FilePath -> IO RevisionId
latest repo' =
  Data.FileStore.latest (gitFileStore repo')

retrieve :: Repo -> FilePath -> Maybe RevisionId -> IO String
retrieve repo' =
  Data.FileStore.retrieve (gitFileStore repo')

save :: Repo -> FilePath -> CommitMessage -> String -> IO ()
save repo' path =
  Data.FileStore.save (gitFileStore repo') path author
  where
    author = Data.FileStore.Author "Arrowsmith" "arrowsmith@no.email"

history :: Repo -> [FilePath] -> Data.FileStore.TimeRange -> Maybe Int -> IO [Data.FileStore.Revision]
history repo' =
  Data.FileStore.history (gitFileStore repo')

commitMessage :: Repo -> RevisionId -> IO String
commitMessage repo' revision =
  liftM Data.FileStore.revDescription (Data.FileStore.revision (gitFileStore repo') revision)

checkout :: Repo -> String -> IO ExitCode
checkout repo' =
  Arrowsmith.Git.gitCheckout (repoPath (repoInfo repo'))

repoHead :: Repo -> IO RevisionId
repoHead repo' =
  Arrowsmith.Git.gitHead (repoPath (repoInfo repo'))

branch :: Repo -> IO String
branch repo' =
  Arrowsmith.Git.gitBranchName (repoPath (repoInfo repo'))

amendCommitMessage :: Repo -> String -> IO ExitCode
amendCommitMessage repo' =
  Arrowsmith.Git.amendCommitMessage (repoPath (repoInfo repo'))

gitFileStore :: Repo -> Data.FileStore.FileStore
gitFileStore repo' =
  Data.FileStore.gitFileStore (repoPath (repoInfo repo'))