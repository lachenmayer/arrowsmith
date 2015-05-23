-- This should mirror src/Arrowsmith/Types.hs
module Arrowsmith.Types where

import Graphics.Element exposing (Element)
import Dict exposing (Dict)

type alias CompileResponse = String
type alias ElmCode = String
type alias ElmError = String
type alias VarName = String
type alias ModuleName = List VarName
type alias ValueViews = Dict VarName ModuleName
type alias Type = String -- TODO...

type alias Definition = (VarName, Maybe Type, ElmCode)

type alias Module =
  { name : ModuleName
  , imports : List Import
  , types : List (VarName, Type)
  , defs : List Definition
  , errors : List ElmError
  }

type CompileStatus
  = Compiled
  | CompileError ElmError

type alias Listing a =
  { explicits : List a
  , open : Bool
  }

type alias ImportMethod =
  { alias : Maybe VarName
  , exposedVars : Listing String
  }

type alias Import =
  (ModuleName, ImportMethod)