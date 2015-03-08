module Arrowsmith.Repo where

import System.FilePath ((</>))

import Arrowsmith.Types

repoPath :: FilePath -> Repo -> FilePath
repoPath basePath repo =
  basePath </> "repos" </> (backend repo) </> (user repo) </> (project repo)
