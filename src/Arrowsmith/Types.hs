{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Types where

import Control.Concurrent.MVar
import Control.Lens.TH
import Data.Aeson.TH
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.FileStore
import Snap.Snaplet (Handler)

-- Serving

data App = App
  { _projects :: MVar String
  }
makeLenses ''App

type AppHandler = Handler App App
type Route = (BS.ByteString, AppHandler ())

-- Elm code

type ElmCode = String
type VarName = String
type QualifiedName = [String]
type Location = (Int {- line -}, Int {- column -}) -- 1-indexed
type Definition = (VarName, Maybe Type, ElmCode)
type PartialDefinition = (Maybe VarName, Maybe Type, Maybe ElmCode)
type LocatedDefinition = (VarName, Maybe Type, ElmCode, Location {- start -}, Location {- end -})
type Type = String

data Module = Module
  { name :: QualifiedName
  , imports :: [String]
  , adts :: [String]
  , defs :: [LocatedDefinition]
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

data Action
  = AddDefinition Definition
  -- | ChangeDefinition VarName ElmCode
  | RemoveDefinition VarName
  deriving (Show, Read, Eq)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''Action)

data EditStatus = EditStatus
  { compileErrors :: [QualifiedName]
  , action :: (QualifiedName, Action)
  } deriving (Show, Read, Eq)

data RepoInfo = RepoInfo
  { backend :: String
  , user :: String
  , project :: String
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''RepoInfo)

type CommitMessage = Data.FileStore.Description
type RevisionId = Data.FileStore.RevisionId

data Repo = Repo
  { repoInfo :: RepoInfo
  , index :: IO [FilePath]
  , latest :: FilePath -> IO RevisionId
  , retrieve :: FilePath -> Maybe RevisionId -> IO String
  , save :: FilePath -> CommitMessage -> String -> IO ()
  }

data Project = Project
  { projectRepo :: RepoInfo
  , sources :: [ElmFile]
  } deriving (Show, Eq)

data ElmFile = ElmFile
  { filePath :: FilePath -- relative to project root
  , fileName :: QualifiedName
  , compiledCode :: Maybe String
  , lastCompiled :: Maybe RevisionId
  , modul :: Maybe Module
  , inRepo :: RepoInfo
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''ElmFile)
