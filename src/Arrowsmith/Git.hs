module Arrowsmith.Git where

import Control.Exception (bracket)
import Data.FileStore.Utils (runShellCommand)
import Data.ByteString.Lazy.UTF8 (toString)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

import Arrowsmith.Paths
import Arrowsmith.Types

-- Stolen from https://hackage.haskell.org/package/filestore-0.6.0.6/docs/src/Data-FileStore-Git.html#gitFileStore
-- | Run a git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, String)
runGitCommand repo command args = do
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  (status, err, out) <- runShellCommand repo env "git" (command : args)
  return (status, toString err, toString out)

gitBranchName :: FilePath -> IO String
gitBranchName repo = do
  (_status, _err, out) <- runGitCommand repo "rev-parse" ["--abbrev-ref", "HEAD"]
  (return . init {- remove trailing \n -}) out

gitHead :: FilePath -> IO RevisionId
gitHead repo = do
  (_status, _err, out) <- runGitCommand repo "rev-parse" ["HEAD"]
  (return . init {- remove trailing \n -}) out

-- Also clears uncommitted changes.
gitCheckout :: FilePath -> String -> IO ExitCode
gitCheckout repo branchOrRevisionId = do
  runGitCommand repo "checkout" ["--", "."]
  (status, _err, _out) <- runGitCommand repo "checkout" [branchOrRevisionId]
  return status

gitClone :: RepoInfo -> IO (ExitCode, String, String)
gitClone repoInfo'
  | backend repoInfo' == "github" =
    runGitCommand "." "clone" ["https://github.com" </> user repoInfo' </> project repoInfo', repoPath repoInfo']

amendCommitMessage :: FilePath -> String -> IO ExitCode
amendCommitMessage repo message = do
  (status, _err, _out) <- runGitCommand repo "commit" ["--amend", "-o", "-m", show message]
  return status

-- TODO very optimistic.
runAtRevision :: FilePath -> String -> IO a -> IO a
runAtRevision repo branchOrRevisionId action = do
  branchHead <- gitBranchName repo
  bracket
    (gitCheckout repo branchOrRevisionId)
    (\_ -> gitCheckout repo branchHead)
    (\_ -> action)

repoRunAtRevision :: Repo -> String -> IO a -> IO a
repoRunAtRevision repo =
  runAtRevision (repoPath (repoInfo repo))
