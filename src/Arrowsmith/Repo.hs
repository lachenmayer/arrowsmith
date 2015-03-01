module Arrowsmith.Repo where

import System.FilePath ((</>))

data Repo = Repo
  { backend :: String
  , user :: String
  , project :: String
  }

repoPath :: FilePath -> Repo -> FilePath
repoPath basePath repo =
  basePath </> "repos" </> (backend repo) </> (user repo) </> (project repo)
