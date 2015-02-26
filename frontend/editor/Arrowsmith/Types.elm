module Arrowsmith.Types where

import Dict (Dict)

type alias CompileResponse = String
type alias CompilationStatus = Result ElmError ElmCode
type alias ElmCode = String
type alias ElmError = String
type alias Name = String
type alias ModuleName = List Name
type alias Value = String
type alias Values = Dict Name Value
type alias Type = String -- TODO...

type alias Definition = 
  { name : Name
  , tipe : Maybe Type
  , binding : ElmCode
  }
  
type alias Module =
  { name : ModuleName
  , imports : List ElmCode
  , adts : List ElmCode
  , defs : List Definition
  }