module Arrowsmith.Repo where

import Control.Monad (liftM)
import qualified Data.FileStore
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode(..))

import qualified Arrowsmith.Git
import Arrowsmith.Paths
import Arrowsmith.Types


getRepo :: RepoInfo -> IO (Either String Repo)
getRepo repoInfo' = do
  let repoPath' = repoPath repoInfo'
  exists <- doesDirectoryExist repoPath'
  -- TODO check if git directory exists
  if exists then
    return $ Right Repo { repoInfo = repoInfo' }
  else do
    (exitCode, err, _out) <- Arrowsmith.Git.gitClone repoInfo'
    case exitCode of
      ExitSuccess -> return $ Right Repo { repoInfo = repoInfo' }
      ExitFailure _ -> return $ Left err

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

allRevisions :: Repo -> FilePath -> IO [RevisionId]
allRevisions repo' file = do
  revisions <- history repo' [file] (Data.FileStore.TimeRange Nothing Nothing) Nothing
  return . map Data.FileStore.revId $ revisions

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