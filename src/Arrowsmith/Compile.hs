module Arrowsmith.Compile (compile) where

import Control.Monad.Error (ErrorT, throwError)
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

compilerPath :: IO FilePath
compilerPath = do
  basePath <- getProgPath
  return $ basePath </> ".." </> "franken-elm" </> "elm-make" </> "dist" </> "build" </> "elm-make" </> "elm-make"

tempDirectory :: FilePath
tempDirectory = "tmp"

compile :: BS.ByteString -> ErrorT String IO (BS.ByteString, BS.ByteString)
compile program = do
  liftIO $ BS.writeFile elmPath program
  (compilerErr, exitCode) <- liftIO $ runCommand tempDirectory compilerPath (compilerFlags elmName jsName)
  case exitCode of
    ExitSuccess -> do
      compiledCode <- liftIO $ BS.readFile jsPath
      ast <- liftIO $ BS.readFile astPath
      return (ast, compiledCode)
    ExitFailure _ -> do
      err <- liftIO $ hGetContents compilerErr
      --throwError err
      throwError . unlines . drop 2 . lines $ err
  where
    programHash = showDigest . sha1 . LazyBS.fromStrict $ program
    elmName = "compiled-program-" ++ programHash <.> "elm"
    jsName = replaceExtension elmName "js"
    elmPath = tempDirectory </> elmName
    jsPath = tempDirectory </> jsName
    compilerFlags inName outName =
      [ inName
      , "--yes"
      , "--output=" ++ outName
      ]

getInterface :: FilePath -> ErrorT String IO Interface
getInterface projectRoot = do
  description <- Desc.read Path.description
  binary <- liftIO . LazyBS.readFile $ projectRoot </> interfacePath description
  return $ Module.interfaceTypes (Binary.decode binary)

stuffDirectory :: Desc.Description -> FilePath
stuffDirectory description =
  Path.stuffDirectory
    </> "build-artifacts"
    </> Name.toFilePath (Desc.name description)
    </> Version.toString (Desc.version description)

astPath :: Desc.Description -> FilePath
astPath description =
  stuffDirectory description </> "Program.elma"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
  exists <- doesFileExist fileName
  if exists
    then removeFile fileName
    else return ()

runCommand :: FilePath -> FilePath -> [String] -> IO (Handle, ExitCode)
runCommand cwd_ cmd args = do
  (_, _, Just stderr, handle) <- createProcess (proc cmd args) { cwd = Just cwd_, std_err = CreatePipe }
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
