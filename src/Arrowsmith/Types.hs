{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Types where

import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.FileStore
import Elm.Package.Description

type ElmCode = String
type Name = String
type QualifiedName = [String]
type Definition = (Name, Maybe Type, ElmCode)
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

data Update
  = AddDefinition QualifiedName {- module name -} Definition
  | ChangeDefinition QualifiedName {- module name -} Name PartialDefinition
  | RemoveDefinition QualifiedName {- module name -} Name
  deriving (Show, Eq)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''Update)

data RepoInfo = RepoInfo
  { backend :: String
  , user :: String
  , project :: String
  }
  deriving (Show, Eq)

type RevisionId = Data.FileStore.RevisionId

data Repo = Repo
  { repoPath :: FilePath
  , repoInfo :: RepoInfo
  , index :: IO [FilePath]
  , latest :: FilePath -> IO RevisionId
  , retrieve :: FilePath -> Maybe RevisionId -> IO LazyBS.ByteString
  }

data Project = Project
  { repo :: Repo
  , description :: Description
  , sources :: [ElmFile]
  }

data ElmFile = ElmFile
  { filePath :: FilePath -- relative to project root
  , fileName :: QualifiedName
  , compiledCode :: Maybe LazyBS.ByteString
  , lastCompiled :: Maybe RevisionId
  , modul :: Maybe Module
  , inRepo :: Repo
  }

data CompileStatus
  = CompileSuccess
  | CompileFailure LazyBS.ByteString