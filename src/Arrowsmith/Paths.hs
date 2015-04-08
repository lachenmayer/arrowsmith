module Arrowsmith.Paths where

import Control.Monad.Error (runErrorT)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), (<.>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Name
import qualified Elm.Package.Paths as ElmPaths
import qualified Elm.Package.Version as Version

import Arrowsmith.Types

-- TODO: set to something sane
rootDirectory :: FilePath
{-# NOINLINE rootDirectory #-}
rootDirectory =
  unsafePerformIO getCurrentDirectory

compilerPath :: FilePath
compilerPath =
  rootDirectory </> ".cabal-sandbox" </> "bin" </> "elm-make"

reposPath :: FilePath
reposPath =
  rootDirectory </> "repos"

repoPath :: RepoInfo -> FilePath
repoPath repoInfo' =
  reposPath </> backend repoInfo' </> user repoInfo' </> project repoInfo'

temporaryDataPath :: FilePath
temporaryDataPath =
  rootDirectory </> "tmp"

temporaryDirectory :: FilePath -> IO FilePath
temporaryDirectory dir = do
  let path = temporaryDataPath </> dir
  createDirectoryIfMissing True path
  return path

getDescription :: FilePath -> IO (Either String Desc.Description)
getDescription projectRoot = do
  let packagePath = projectRoot </> ElmPaths.description
  runErrorT $ Desc.read packagePath

stuffDirectory :: Desc.Description -> FilePath
stuffDirectory description' =
  ElmPaths.stuffDirectory
    </> "build-artifacts"
    </> Name.toFilePath (Desc.name description')
    </> Version.toString (Desc.version description')

astPath :: Desc.Description -> QualifiedName -> FilePath
astPath description' moduleName =
  stuffDirectory description' </> intercalate "-" moduleName <.> "elma"
