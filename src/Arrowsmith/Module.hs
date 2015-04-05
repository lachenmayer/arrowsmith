module Arrowsmith.Module where

import Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import Data.List (tails, isPrefixOf)
import Data.List.Split (splitOn)
--import qualified Data.ByteString.Lazy.Char8 as C8
--import qualified Data.ByteString.UTF8 as UTF8BS

-- elm-compiler
import qualified AST.Annotation as Annotation
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.JSON ()
import qualified AST.Module
import qualified AST.Pattern as Pattern
import qualified AST.PrettyPrint as PP
--import qualified AST.Variable as Var

import Arrowsmith.Types

-- Converts an elm-compiler Def to an Arrowsmith Definition.
type DefTransform = Canonical.Def -> Definition

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
        _ -> error "unexpected AST structure. (letDefs)"

-- Uses the built-in pretty printer to give a textual representation of the definition.
defPrettyPrinted :: DefTransform
defPrettyPrinted def =
  ( varName
  , tipe >>= Just . PP.renderPretty
  , PP.renderPretty binding
  , startPosition
  , endPosition
  )
  where
    Canonical.Definition (Pattern.Var varName) binding tipe = def
    (startPosition, endPosition) = sourceRange def

-- Looks up the regions in the code itself to match the style the code was written in originally.
defFromSource :: String -> DefTransform
defFromSource source def =
  ( varName
  , tipe >>= Just . PP.renderPretty
  , sourceRegion source lhsStartPosition endPosition
  , lhsStartPosition
  , endPosition
  )
  where
    Canonical.Definition (Pattern.Var varName) _ tipe = def
    (startPosition, endPosition) = sourceRange def
    lhsStartPosition = expandToLhs source varName startPosition


-- 1-indexed
-- indexOf "foobarbazbar" "bar" == Just 4
indexOf :: String -> String -> Maybe Int
indexOf haystack needle =
  if null results then Nothing else Just ((snd . head) results)
  where
    results = filter fst $ zip (map (isPrefixOf needle) (tails haystack)) [1..]


-- Returns the start position of a definition including the left hand side.
-- expandToLhs "foo\n  baz\nbal = baz\nbar" "baz" (3, 6) == (2, 3)
expandToLhs :: String -> String -> Position -> Position
expandToLhs source varName (startLine, startColumn) =
  case indexOf (take startColumn firstDefLine) varName of
    Just i -> (startLine, i)
    Nothing -> findInPrevious previousLines (startLine - 1)
  where
    (firstDefLine:previousLines) = reverse $ take startLine (lines source)
    findInPrevious _ 0 = error "couldn't find def (expandToLhs:1)"
    findInPrevious [] _ = error "couldn't find def (expandToLhs:2)"
    findInPrevious (line:previous) lineCount =
      case indexOf line varName of
        Just i -> (lineCount, i)
        Nothing -> findInPrevious previous (lineCount - 1)

unpos p =
  (Annotation.line p, Annotation.column p)

sourceRange :: Canonical.Def -> (Position, Position)
sourceRange def =
  (unpos startPosition, unpos endPosition)
  where
    Canonical.Definition (Pattern.Var _) binding _ = def
    Annotation.A (Annotation.Span startPosition endPosition _) _ = binding

sourceRegion :: String -> Position -> Position -> String
sourceRegion source (startLine, startColumn) (endLine, endColumn) =
  columnRange (region startLine endLine (lines source))
  where
    -- Lines are 1-indexed.
    columnRange lines' =
      case lines' of
        [] -> ""
        [x] -> region startColumn endColumn x
        (x:xs) ->
          let (middle, lastLine) = (init xs, last xs) in
            unlines $ [drop (startColumn - 1) x] ++ middle ++ [take endColumn lastLine]

breakSource :: String -> Position -> Position -> (String, String, String)
breakSource source start end =
  (before, def, after)
  where
    (before:after:_) = splitOn def source -- let's hope the def only appears once...
    def = sourceRegion source start end

-- returns the sub-list bounded by `start` and `end` (1-indexed)
region :: Int -> Int -> [a] -> [a]
region start end xs =
  take ((end - start) + 1) $ drop (start - 1) xs
