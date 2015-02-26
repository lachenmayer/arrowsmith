{-# LANGUAGE FlexibleContexts #-}
module Arrowsmith.Compile (compile) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Map as Map
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Directory (doesFileExist, removeFile)
import System.Environment.FindBin (getProgPath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), replaceExtension)
import System.IO (Handle, hGetContents)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Name
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as Version

compilerPath :: FilePath -> FilePath
compilerPath projectRoot =
  projectRoot </> ".cabal-sandbox" </> "bin" </> "elm-make"

elmRoot :: String -> String -> String -> String -> FilePath
elmRoot projectRoot backend user project =
  projectRoot </> "repos" </> backend </> user </> project

compile :: (MonadIO m, MonadError String m) => String -> String -> String -> String -> m (BS.ByteString, BS.ByteString)
compile backend user project moduleName = do
  projectRoot <- liftIO getProgPath
  let compileDirectory = elmRoot projectRoot backend user project
  let command = compilerPath projectRoot
  (compilerErr, exitCode) <- liftIO $ runCommand compileDirectory command (compilerFlags moduleName)
  case exitCode of
    ExitSuccess -> do
      compiledCode <- liftIO $ BS.readFile (compileDirectory </> "elm.js")
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

--compile :: BS.ByteString -> ErrorT String IO (BS.ByteString, BS.ByteString)
--compile program = do
--  liftIO $ BS.writeFile elmPath program
--  (compilerErr, exitCode) <- liftIO $ runCommand tempDirectory compilerPath (compilerFlags elmName jsName)

getAST :: (MonadIO m, MonadError String m) => FilePath -> String -> m BS.ByteString
getAST projectRoot moduleName = do
  description <- Desc.read $ projectRoot </> Path.description
  liftIO $ BS.readFile $ projectRoot </> astPath description moduleName

astPath :: Desc.Description -> String -> FilePath
astPath description moduleName =
  stuffDirectory description </> moduleName <.> "elma"

stuffDirectory :: Desc.Description -> FilePath
stuffDirectory description =
  Path.stuffDirectory
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

--helloWorld :: Program
--helloWorld =
--  Program
--    { imports =
--      [ "import Graphics.Element (..)"
--      , "import Text (..)"
--      ]
--    , adts = []
--    , defs =
--      [ "main : Element\nmain = plainText \"Hello, World!\""
--      ]
--    }
