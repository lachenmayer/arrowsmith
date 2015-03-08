{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Types where

import Data.Aeson.TH

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

data Repo = Repo
  { backend :: String
  , user :: String
  , project :: String
  }
  deriving (Show, Eq)