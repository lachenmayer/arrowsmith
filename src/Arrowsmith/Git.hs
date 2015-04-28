module Arrowsmith.Git where

import Control.Exception (bracket)
import Data.FileStore (RevisionId)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (toString)
import System.Exit (ExitCode(..))


-- Stolen from https://hackage.haskell.org/package/filestore-0.6.0.6/docs/src/Data-FileStore-Git.html#gitFileStore
-- | Run a git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommand repo command args = do
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  (status, err, out) <- runShellCommand repo env "git" (command : args)
  return (status, toString err, out)

gitBranchName :: FilePath -> IO String
gitBranchName repo = do
  (_status, _err, out) <- runGitCommand repo "rev-parse" ["--abbrev-ref", "HEAD"]
  (return . init {- remove trailing \n -} . toString) out

gitHead :: FilePath -> IO RevisionId
gitHead repo = do
  (_status, _err, out) <- runGitCommand repo "rev-parse" ["HEAD"]
  (return . init {- remove trailing \n -} . toString) out

-- Also clears uncommitted changes.
gitCheckout :: FilePath -> String -> IO ExitCode
gitCheckout repo branchOrRevisionId = do
  runGitCommand repo "checkout" ["--", "."]
  (status, _err, _out) <- runGitCommand repo "checkout" [branchOrRevisionId]
  return status

-- TODO very optimistic.
runAtRevision :: FilePath -> String -> IO a -> IO a
runAtRevision repo branchOrRevisionId action = do
  branchHead <- gitBranchName repo
  bracket
    (gitCheckout repo branchOrRevisionId)
    (\_ -> gitCheckout repo branchHead)
    (\_ -> action)
