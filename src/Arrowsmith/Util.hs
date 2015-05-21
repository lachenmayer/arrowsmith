module Arrowsmith.Util where

import Control.Monad (when)
import Data.List (tails, isPrefixOf, minimumBy, stripPrefix)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist, removeFile)
import System.FilePath.Posix (joinPath, normalise, splitDirectories)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
  exists <- doesFileExist fileName
  when exists (removeFile fileName)

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
stripLongestPrefix dirs path
  | dirs == [] = path
  | suffixes == [] = path
  | otherwise = joinPath . minimumBy (comparing (length . concat)) $ suffixes
  where
    suffixes = mapMaybe (`stripPrefix` splitPath) splitDirs
    splitPath = splitDirectories path
    splitDirs = map splitDirectories . replace "." "" . map normalise $ dirs

-- 1-indexed
-- indexOf "foobarbazbar" "bar" == Just 4
indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf haystack needle =
  if null results then Nothing else Just ((snd . head) results)
  where
    results = filter fst $ zip (map (isPrefixOf needle) (tails haystack)) [1..]

-- returns the sub-list bounded by `start` and `end` (1-indexed)
region :: Int -> Int -> [a] -> [a]
region start end xs =
  take ((end - start) + 1) $ drop (start - 1) xs

update :: Eq a => a -> a -> [a] -> [a]
update old new =
  map (\x -> if x == old then new else x)