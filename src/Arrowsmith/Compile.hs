{-# LANGUAGE FlexibleContexts #-}
module Arrowsmith.Compile (compile) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
--import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hGetContents)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)

import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Name
import qualified Elm.Package.Paths as ElmPaths
import qualified Elm.Package.Version as Version

import Arrowsmith.Repo (Repo, repoPath)

compilerPath :: FilePath -> FilePath
compilerPath projectRoot =
  projectRoot </> ".cabal-sandbox" </> "bin" </> "elm-make"

compile :: (MonadIO m, MonadError String m) => Repo -> String -> m (BS.ByteString, BS.ByteString)
compile repo moduleName = do
  basePath <- liftIO getCurrentDirectory -- everything will break if we don't execute from the project root
  let projectRoot = repoPath basePath repo
  let command = compilerPath basePath
  (compilerErr, exitCode) <- liftIO $ runCommand projectRoot command (compilerFlags moduleName)
  case exitCode of
    ExitSuccess -> do
      compiledCode <- liftIO $ BS.readFile (projectRoot </> "elm.js")
      ast <- getAST projectRoot moduleName
      return (ast, compiledCode)
    ExitFailure _ -> do
      err <- liftIO $ hGetContents compilerErr
      --throwError err
      throwError . unlines . drop 2 . lines $ err
  where
    compilerFlags inName =
      [ inName <.> "elm"
      , "--yes"
      --, "--output=" ++ outName
      ]

getAST :: (MonadIO m, MonadError String m) => FilePath -> String -> m BS.ByteString
getAST projectRoot moduleName = do
  description <- Desc.read $ projectRoot </> ElmPaths.description
  liftIO . BS.readFile $ projectRoot </> astPath description moduleName

astPath :: Desc.Description -> String -> FilePath
astPath description moduleName =
  stuffDirectory description </> moduleName <.> "elma"

stuffDirectory :: Desc.Description -> FilePath
stuffDirectory description =
  ElmPaths.stuffDirectory
    </> "build-artifacts"
    </> Name.toFilePath (Desc.name description)
    </> Version.toString (Desc.version description)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
  exists <- doesFileExist fileName
  if exists
    then removeFile fileName
    else return ()

runCommand :: FilePath -> FilePath -> [String] -> IO (Handle, ExitCode)
runCommand workingDirectory command args = do
  (_, _, Just stderr, handle) <- createProcess (proc command args) { cwd = Just workingDirectory, std_err = CreatePipe }
  exitCode <- waitForProcess handle
  return (stderr, exitCode)
