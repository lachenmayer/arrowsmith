module Arrowsmith.Paths where

import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Name
import qualified Elm.Package.Paths as ElmPaths
import qualified Elm.Package.Version as Version

-- TODO: set to something sane
rootDirectory :: FilePath
rootDirectory =
  unsafePerformIO getCurrentDirectory

compilerPath :: FilePath
compilerPath =
  rootDirectory </> ".cabal-sandbox" </> "bin" </> "elm-make"

reposPath :: FilePath
reposPath =
  rootDirectory </> "repos"

temporaryDataPath :: FilePath
temporaryDataPath =
  rootDirectory </> "tmp"

temporaryDirectory :: FilePath -> IO FilePath
temporaryDirectory dir = do
  let path = temporaryDataPath </> dir
  createDirectoryIfMissing True path
  return path

stuffDirectory :: Desc.Description -> FilePath
stuffDirectory description =
  ElmPaths.stuffDirectory
    </> "build-artifacts"
    </> Name.toFilePath (Desc.name description)
    </> Version.toString (Desc.version description)