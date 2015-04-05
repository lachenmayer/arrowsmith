{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Types where

import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.FileStore

type ElmCode = String
type Name = String
type QualifiedName = [String]
type Position = (Int {- line -}, Int {- column -}) -- 1-indexed
type Definition = (Name, Maybe Type, ElmCode, Position {- start -}, Position {- end -})
type QualifiedDefinition = (QualifiedName, Maybe Type, ElmCode)
type PartialDefinition = (Maybe Name, Maybe Type, Maybe ElmCode)
type Type = String

data Module = Module
  { name :: QualifiedName
  , imports :: [String]
  , adts :: [String]
  , defs :: [Definition]
  }
  deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

data Action
  = AddDefinition QualifiedName {- module name -} Definition
  | ChangeDefinition QualifiedName {- module name -} Name PartialDefinition
  | RemoveDefinition QualifiedName {- module name -} Name
  deriving (Show, Eq)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''Action)

data RepoInfo = RepoInfo
  { backend :: String
  , user :: String
  , project :: String
  }
  deriving (Show, Eq)
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
  }
  deriving (Show, Eq)

data ElmFile = ElmFile
  { filePath :: FilePath -- relative to project root
  , fileName :: QualifiedName
  , compiledCode :: Maybe String
  , lastCompiled :: Maybe RevisionId
  , lastEdited :: Maybe RevisionId
  , modul :: Maybe Module
  , inRepo :: RepoInfo
  }
  deriving (Show, Eq)
$(deriveJSON defaultOptions ''ElmFile)
