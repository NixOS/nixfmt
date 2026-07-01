{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Postprocess (applyDirectives) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Given the original input and the formatted output, find /*nixfmt:disable*/ / /*nixfmt:enable*/
-- directive pairs and replace formatted regions with the corresponding raw input.
--
-- Directives must be on their own line (only optional leading whitespace).
-- Directives inside strings are ignored, but directives inside ${} interpolations
-- are recognized (since interpolations contain real Nix code).
-- Unclosed /*nixfmt:disable*/ extends to end of file.
applyDirectives :: Text -> Text -> Text
applyDirectives original formatted =
  let originalLines = Text.lines original
      formattedLines = Text.lines formatted
      originalRegions = findRegions originalLines
      formattedRegions = findRegions formattedLines
  in if null formattedRegions
       then formatted
       else Text.unlines $ spliceRegions originalLines originalRegions formattedLines formattedRegions

-- | A region is a (startLine, endLine) pair where startLine is the /*nixfmt:disable*/ line
-- and endLine is the /*nixfmt:enable*/ line (or Nothing for unclosed regions).
type Region = (Int, Maybe Int)

-- | Scanning state frame. The scanner uses a stack of frames to track
-- nested contexts like string interpolations.
data ScanFrame = FNormal | FString | FMultiString | FBlockComment
  deriving (Eq)

-- | The scan state is a stack of frames. The head is the current context.
-- When entering an interpolation (${) from a string, we push FNormal.
-- When entering a nested {} block, we push FNormal.
-- When the matching } is found, we pop back to the enclosing context.
type ScanState = [ScanFrame]

isNormal :: ScanState -> Bool
isNormal (FNormal : _) = True
isNormal [] = True -- empty stack treated as normal
isNormal _ = False

-- | Find all disable/enable regions in a list of lines.
-- Returns regions in order. Nested disables are ignored.
-- Uses a lightweight lexer to skip directives inside strings and block comments.
findRegions :: [Text] -> [Region]
findRegions = collectRegions . scanDirectives

-- | Scan lines for directive comments, respecting string/comment context.
-- Returns a list of (lineIndex, isDisable) for real directives only.
scanDirectives :: [Text] -> [(Int, Bool)]
scanDirectives = go [FNormal] 0
  where
    go _ _ [] = []
    go state lineIdx (line : rest) =
      let directives
            | isNormal state, isDisableDirective line = [(lineIdx, True)]
            | isNormal state, isEnableDirective line = [(lineIdx, False)]
            | otherwise = []
          newState = advanceState state (Text.unpack line)
      in directives ++ go newState (lineIdx + 1) rest

-- | Collect disable/enable pairs from a list of directive positions.
collectRegions :: [(Int, Bool)] -> [Region]
collectRegions = go Nothing
  where
    go Nothing [] = []
    go (Just start) [] = [(start, Nothing)]
    go Nothing ((i, True) : rest) = go (Just i) rest
    go Nothing ((_, False) : rest) = go Nothing rest -- lone enable, ignore
    go (Just start) ((i, False) : rest) = (start, Just i) : go Nothing rest
    go (Just start) ((_, True) : rest) = go (Just start) rest -- nested disable, ignore

-- | Advance the scan state by processing a line character by character.
-- Uses a stack to track nested contexts: strings, interpolations, block comments.
advanceState :: ScanState -> [Char] -> ScanState
-- Normal mode
advanceState s@(FNormal : _) [] = s
advanceState s@(FNormal : _) ('#' : _) = s -- line comment, rest is irrelevant
advanceState (FNormal : outer) ('"' : rest) = advanceState (FString : FNormal : outer) rest
advanceState (FNormal : outer) ('\'' : '\'' : rest) = advanceState (FMultiString : FNormal : outer) rest
advanceState (FNormal : outer) ('/' : '*' : rest) = advanceState (FBlockComment : FNormal : outer) rest
advanceState (FNormal : outer) ('{' : rest) = advanceState (FNormal : FNormal : outer) rest -- nested braces
advanceState s@(FNormal : _) ('}' : rest) = advanceState (pop s) rest -- close brace/interpolation
advanceState s@(FNormal : _) (_ : rest) = advanceState s rest
-- Regular string ("...")
advanceState s@(FString : _) [] = s
advanceState s@(FString : _) ('\\' : _ : rest) = advanceState s rest -- escape sequence
advanceState s@(FString : _) ('$' : '{' : rest) = advanceState (FNormal : s) rest -- interpolation
advanceState (FString : outer) ('"' : rest) = advanceState outer rest -- closing "
advanceState s@(FString : _) (_ : rest) = advanceState s rest
-- Multi-line string (''...'')
advanceState s@(FMultiString : _) [] = s
advanceState s@(FMultiString : _) ('\'' : '\'' : '\'' : rest) = advanceState s rest -- ''' escape
advanceState s@(FMultiString : _) ('\'' : '\'' : '\\' : rest) = advanceState s rest -- ''\ escape
advanceState s@(FMultiString : _) ('\'' : '\'' : '$' : '{' : rest) = advanceState s rest -- ''${ escape
advanceState (FMultiString : outer) ('\'' : '\'' : rest) = advanceState outer rest -- closing ''
advanceState s@(FMultiString : _) ('$' : '{' : rest) = advanceState (FNormal : s) rest -- interpolation
advanceState s@(FMultiString : _) (_ : rest) = advanceState s rest
-- Block comment (/* ... */)
advanceState s@(FBlockComment : _) [] = s
advanceState (FBlockComment : outer) ('*' : '/' : rest) = advanceState outer rest
advanceState s@(FBlockComment : _) (_ : rest) = advanceState s rest
-- Empty stack fallback (shouldn't happen in well-formed input)
advanceState [] _ = []

-- | Pop one frame from the stack. Never pops below a single FNormal.
pop :: ScanState -> ScanState
pop (_ : rest@(_ : _)) = rest
pop s = s -- don't pop the last frame

isDisableDirective :: Text -> Bool
isDisableDirective = isDirective "disable"

isEnableDirective :: Text -> Bool
isEnableDirective = isDirective "enable"

isDirective :: Text -> Text -> Bool
isDirective directive line =
  let stripped = Text.strip line
  in stripped == "/*nixfmt:" <> directive <> "*/"

-- | Splice original regions into formatted lines, matching regions positionally.
spliceRegions :: [Text] -> [Region] -> [Text] -> [Region] -> [Text]
spliceRegions originalLines originalRegions formattedLines formattedRegions =
  go 0 (zip originalRegions formattedRegions) formattedLines
  where
    go _ [] remaining = remaining
    go lineIdx ((origRegion, fmtRegion) : rest) remaining =
      let (fmtStart, fmtEnd) = regionBounds fmtRegion (length formattedLines)
          (origStart, origEnd) = regionBounds origRegion (length originalLines)
          -- Lines before this region (from current position)
          prefixCount = fmtStart - lineIdx
          (prefix, afterPrefix) = splitAt prefixCount remaining
          -- Lines in the formatted region (to skip)
          fmtRegionLen = fmtEnd - fmtStart + 1
          (_, afterRegion) = splitAt fmtRegionLen afterPrefix
          -- Lines from the original to splice in
          origChunk = take (origEnd - origStart + 1) $ drop origStart originalLines
      in prefix ++ origChunk ++ go (fmtEnd + 1) rest afterRegion

    regionBounds :: Region -> Int -> (Int, Int)
    regionBounds (start, Just end) _ = (start, end)
    regionBounds (start, Nothing) totalLines = (start, totalLines - 1)
