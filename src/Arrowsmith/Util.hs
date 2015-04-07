module Arrowsmith.Util where

import Data.List (sortBy, stripPrefix)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist, removeFile)
import System.FilePath.Posix (joinPath, normalise, splitDirectories)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
  exists <- doesFileExist fileName
  if exists
    then removeFile fileName
    else return ()

runCommand :: FilePath -> FilePath -> [String] -> IO (Handle, Handle, ExitCode)
runCommand workingDirectory command args = do
  (_, Just stdout, Just stderr, handle) <- createProcess (proc command args) { cwd = Just workingDirectory, std_err = CreatePipe, std_out = CreatePipe }
  exitCode <- waitForProcess handle
  return (stdout, stderr, exitCode)

replace :: Eq a => a -> a -> [a] -> [a]
replace element with =
  map (\x -> if x == element then with else x)

-- stripLongestPrefix [".", "src"] "src/Foo.elm" == "Foo.elm"
stripLongestPrefix :: [FilePath] -> FilePath -> FilePath
stripLongestPrefix [] path =
  path
stripLongestPrefix dirs path =
  joinPath . head . sortBy (comparing (length . concat)) . catMaybes . map (flip stripPrefix splitPath) $ splitDirs
  where
    splitPath = splitDirectories path
    splitDirs = map splitDirectories . replace "." "" . map normalise $ dirs