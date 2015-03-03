{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Module where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LazyBS
--import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.UTF8 as UTF8BS

-- elm-compiler
import qualified AST.Annotation as Annotation
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.JSON ()
import qualified AST.Module
import qualified AST.Pattern as Pattern
import qualified AST.PrettyPrint as PP
--import qualified AST.Variable as Var


type Definition =
  ( String -- name
  , Maybe String -- type
  , String -- binding
  )

-- Converts an elm-compiler Def to an Arrowsmith Definition.
type DefTransform = Canonical.Def -> Definition

data Module = Module
  { name :: AST.Module.Name
  , imports :: [String]
  , adts :: [String]
  , defs :: [Definition]
  }
  deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

-- Converts an elm-compiler Module to an Arrowsmith Module.
type ModuleTransform = AST.Module.CanonicalModule -> Module

makeModule :: DefTransform -> ModuleTransform
makeModule defTransform m =
  Module { name = AST.Module.names m
         , imports = []
         , adts = []
         , defs = map defTransform (definitions m)
         }

modulePrettyPrintedDefs :: ModuleTransform
modulePrettyPrintedDefs =
  makeModule defPrettyPrinted

moduleSourceDefs :: String -> ModuleTransform
moduleSourceDefs source =
  makeModule (defFromSource source)

fromAstFile :: LazyBS.ByteString -> Maybe AST.Module.CanonicalModule
fromAstFile astFile =
  (decode astFile :: Maybe AST.Module.CanonicalModule)

definitions :: AST.Module.CanonicalModule -> [Canonical.Def]
definitions modoole =
  reverse $ letDefs program_
  where
    program_ = AST.Module.program (AST.Module.body modoole)

    -- Definitions are in one big 'let' block, turn them into a list.
    letDefs (Annotation.A _ ds) =
      case ds of
        General.Var _ -> [] -- should be the "_save_the_environment!!!" varaible.
        General.Let [def] next -> def : letDefs next
        _ -> error "unexpected AST structure. (2)"
    letDefs _ =
      error "unexpected AST structure. (1)"

-- Uses the built-in pretty printer to give a textual representation of the definition.
defPrettyPrinted :: DefTransform
defPrettyPrinted def =
  ( varName
  , tipe >>= Just . PP.renderPretty
  , PP.renderPretty binding
  )
  where
    Canonical.Definition (Pattern.Var varName) binding tipe = def
defPrettyPrinted _ =
  ("___not_implemented!!!", Nothing, "not implemented! (Arrowsmith.Module.defPrettyPrinted)")

-- Looks up the regions in the code itself to match the style the code was written in originally.
defFromSource :: String -> DefTransform
defFromSource source def =
  ( varName
  , tipe >>= Just . PP.renderPretty
  , sourceRegion source (unpos startPosition) (unpos endPosition)
  )
  where
    Canonical.Definition (Pattern.Var varName) binding tipe = def
    Annotation.A (Annotation.Span startPosition endPosition _) _ = binding
    unpos p = (Annotation.line p, Annotation.column p)

sourceRegion :: String -> (Int, Int) -> (Int, Int) -> String
sourceRegion source (startLine, startColumn) (endLine, endColumn) =
  columnRange (region startLine endLine (lines source))
  where
    -- Lines are 1-indexed.
    region start end xs = take ((end - start) + 1) $ drop (start - 1) xs
    columnRange lines' =
      case lines' of
        [] -> ""
        [x] -> region startColumn endColumn x
        (x:xs) ->
          let (middle, lastLine) = (init xs, last xs) in
            unlines $ [drop (startColumn - 1) x] ++ middle ++ [take endColumn lastLine]
