module Arrowsmith.Module where

import Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import Data.Function (on)
import Data.List (intercalate, sortBy)
import qualified Data.Map as Map

-- elm-compiler
import qualified AST.Annotation as Annotation
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.JSON ()
import qualified AST.Module
import qualified AST.Pattern as Pattern
import qualified AST.PrettyPrint as PP
import qualified AST.Type
import qualified AST.Variable as Var

import Arrowsmith.Types
import Arrowsmith.Util

-- Converts an elm-compiler Def to an Arrowsmith Definition.
type DefTransform = Canonical.Def -> LocatedDefinition

-- Converts an elm-compiler Module to an Arrowsmith Module.
type ModuleTransform = AST.Module.CanonicalModule -> Module

makeModule :: DefTransform -> ModuleTransform
makeModule defTransform m =
  Module
    { name = AST.Module.names m
    , imports = moduleImports m
    , types = moduleTypes m
    , datatypes = moduleDatatypes m
    , aliases = moduleAliases m
    , defs = sortByLocation $ map defTransform (definitions m)
    , errors = []
    }

modulePrettyPrintedDefs :: ModuleTransform
modulePrettyPrintedDefs =
  makeModule defPrettyPrinted

moduleSourceDefs :: String -> ModuleTransform
moduleSourceDefs source' =
  makeModule . defFromSource $ source'

sortByLocation :: [LocatedDefinition] -> [LocatedDefinition]
sortByLocation =
  sortBy (compare `on` (\x -> let (_, _, _, (startLine, _), _) = x in startLine))

fromAstFile :: LazyBS.ByteString -> Maybe AST.Module.CanonicalModule
fromAstFile astFile =
  decode astFile :: Maybe AST.Module.CanonicalModule

definitions :: AST.Module.CanonicalModule -> [Canonical.Def]
definitions modoole =
  reverse $ letDefs program_
  where
    program_ = AST.Module.program (AST.Module.body modoole)

    -- Definitions are in one big 'let' block, turn them into a list.
    letDefs (Annotation.A _ ds) =
      case ds of
        General.Var _ -> [] -- should be the "_save_the_environment!!!" variable.
        -- Any "normal" definition (ie. not an alias/type definition) will look like this.
        General.Let [def@(Canonical.Definition (Pattern.Var _) (Annotation.A (Annotation.Span _ _ _) _) _)] next -> def : letDefs next
        -- Reject all others - they don't have location information.
        General.Let [_def] next -> letDefs next
        _ -> error "unexpected AST structure. (letDefs)"

-- Uses the built-in pretty printer to give a textual representation of the definition.
defPrettyPrinted :: DefTransform
defPrettyPrinted def =
  ( varName'
  , tipe >>= Just . convertType
  , PP.renderPretty binding
  , startLocation
  , endLocation'
  )
  where
    Canonical.Definition (Pattern.Var varName') binding tipe = def
    (startLocation, endLocation') = sourceRange def

-- Looks up the regions in the code itself to match the style the code was written in originally.
defFromSource :: String -> DefTransform
defFromSource source' def =
  ( varName'
  , tipe >>= Just . convertType
  , sourceRegion source' lhsStartLocation eolEndLocation
  , lhsStartLocation
  , eolEndLocation
  )
  where
    Canonical.Definition (Pattern.Var varName') _ tipe = def
    (startLocation, endLocation') = sourceRange def
    lhsStartLocation = expandToLhs source' varName' startLocation
    eolEndLocation = expandToEndOfLine source' endLocation'

-- Returns the start position of a definition including the left hand side.
-- expandToLhs "foo\n  baz\nbal = baz\nbar" "baz" (3, 6) == (2, 3)
expandToLhs :: String -> String -> Location -> Location
expandToLhs source' varName' (startLine, startColumn) =
  case indexOf (take startColumn firstDefLine) varName' of
    Just i -> (startLine, i)
    Nothing -> findInPrevious previousLines (startLine - 1)
  where
    (firstDefLine:previousLines) = reverse $ take startLine (lines source')
    findInPrevious _ 0 = error "couldn't find def (expandToLhs:1)"
    findInPrevious [] _ = error "couldn't find def (expandToLhs:2)"
    findInPrevious (line:previous) lineCount =
      case indexOf line varName' of
        Just i -> (lineCount, i)
        Nothing -> findInPrevious previous (lineCount - 1)

expandToEndOfLine :: String -> Location -> Location
expandToEndOfLine source' (line, _) =
  (line, length (lines source' !! (line - 1)) + 1)

toLocation :: Annotation.Position -> Location
toLocation p =
  (Annotation.line p, Annotation.column p)

sourceRange :: Canonical.Def -> (Location, Location)
sourceRange def =
  (toLocation startPosition, toLocation endPosition)
  where
    Canonical.Definition (Pattern.Var _) binding _ = def
    Annotation.A (Annotation.Span startPosition endPosition _) _ = binding

sourceRegion :: String -> Location -> Location -> String
sourceRegion source' (startLine, startColumn) (endLine, endColumn) =
  columnRange (region startLine endLine (lines source'))
  where
    -- Lines are 1-indexed.
    columnRange lines' =
      case lines' of
        [] -> ""
        [x] -> region startColumn (endColumn - 1) x
        (firstLine:remainingLines) ->
          let (middle, lastLine) = (init remainingLines, last remainingLines) in
            unlines $ [drop (startColumn - 1) firstLine] ++ middle ++ [take (endColumn - 1) lastLine]

-- (before, definition (bounded by locations), after)
breakSource :: String -> Location -> Location -> (String, String, String)
breakSource source' (startLine, startCol) (endLine, endCol) =
  (unlines linesBefore ++ firstLineBefore, (unlines . init) defLines ++ lastDefLine, lastLineAfter ++ unlines linesAfter)
  where
    idx x = max 0 (x - 1)
    sourceLines = lines source'
    (linesBefore, (firstLine:remainingLines)) = splitAt (idx startLine) sourceLines
    (firstLineBefore, firstDefLine) = splitAt (idx startCol) firstLine
    (defLines, linesAfter) = splitAt (endLine - idx startLine) (firstDefLine:remainingLines)
    (lastDefLine, lastLineAfter) = splitAt endCol (last defLines)

endLocation :: Location -> String -> Location
endLocation (startRow, _) def =
  (startRow + length defLines - 1, length lastLine)
  where
    defLines = lines def
    lastLine = last defLines

moduleTypes :: AST.Module.CanonicalModule -> [(VarName, Type)]
moduleTypes modoole =
  map (\(n, tipe) -> (n, convertType tipe)) . Map.toList $ AST.Module.types (AST.Module.body modoole)

moduleDatatypes :: AST.Module.CanonicalModule -> [(VarName, AdtInfo)]
moduleDatatypes modoole =
  map (\(n, adtInfo) -> (n, pretty adtInfo)) . Map.toList $ AST.Module.datatypes (AST.Module.body modoole)
  where
    pretty (s, constructors') =
      AdtInfo s $ map (\(v, ts) -> (v, map convertType ts)) constructors'

moduleAliases :: AST.Module.CanonicalModule -> [(VarName, (ModuleName, Type))]
moduleAliases modoole =
  map (\(n, (m, t)) -> (n, (m, convertType t))) . Map.toList $ AST.Module.aliases (AST.Module.body modoole)

moduleImports :: AST.Module.CanonicalModule -> [Import]
moduleImports modoole =
  map importFromElm (AST.Module.imports modoole)
  where
    importFromElm (name', importMethod) =
      (name', importMethodFromElm importMethod)
    importMethodFromElm (AST.Module.ImportMethod alias' exposedVars') =
      ImportMethod alias' (listingFromElm exposedVars')
    listingFromElm (Var.Listing explicits' open') =
      Listing (map valueToString explicits') open'
    valueToString (Var.Value str) = str
    valueToString (Var.Alias str) = str
    valueToString (Var.Union str (Var.Listing explicits' open')) =
      str ++ "(" ++ (intercalate ", " $ explicits' ++ [openDots]) ++ ")"
      where
        openDots = if open' then ".." else ""

convertType :: AST.Type.CanonicalType -> Type
convertType tipe =
  case tipe of
    AST.Type.Lambda t1 t2 -> Lambda (convertType t1) (convertType t2)
    AST.Type.Var s -> Var s
    AST.Type.Type v -> Type (convertCanonical v)
    AST.Type.App t ts -> TypeApp (convertType t) (map convertType ts)
    AST.Type.Record fs maybeType -> Record (convertNamed fs) (fmap convertType maybeType)
    AST.Type.Aliased v fs a -> Aliased (convertCanonical v) (convertNamed fs) (convertAliasType a)
  where
    convertNamed = map (\(n, t) -> (n, convertType t))

convertCanonical :: Var.Canonical -> CanonicalVar
convertCanonical (Var.Canonical h n) =
  CanonicalVar (convertHome h) n

convertHome :: Var.Home -> Home
convertHome h =
  case h of
    Var.BuiltIn -> BuiltIn
    Var.Module m -> ModuleHome m
    Var.Local -> Local

convertAliasType :: AST.Type.AliasType Var.Canonical -> AliasType
convertAliasType a =
  case a of
    AST.Type.Holey t -> Holey (convertType t)
    AST.Type.Filled t -> Filled (convertType t)
