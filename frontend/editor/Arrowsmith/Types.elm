-- This should mirror src/Arrowsmith/Types.hs
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

type alias Definition = (Name, Maybe Type, ElmCode)

type alias Module =
  { name : ModuleName
  , imports : List ElmCode
  , adts : List ElmCode
  , defs : List Definition
  }

type alias State =
  { modul : Module

  , isCompiling : Bool
  , compilationStatus : CompilationStatus
  , dirty : Bool -- The code has changed (eg. by editing), but it has not been recompiled yet.

  , editing : Maybe Name

  , values : Dict Name Value
  , toEvaluate : Maybe (ModuleName, Name)

  , fresh : Int
  }

type Action
  = NoOp

  | Compile Module
  | FinishCompiling (CompileResponse, String)

  | Edit Name
  | StopEditing Name
  | FinishEditing (Name, Value)

  | Evaluate (ModuleName, Name)
  | FinishEvaluating (ModuleName, Name, Value)

  | NewDefinition
  | RemoveDefinition Name