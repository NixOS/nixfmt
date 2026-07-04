{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Nixfmt.Lexer (lexeme, pushTrivia, takeTrivia, whole, wholeInner) where

import Control.Monad (guard)
import Control.Monad.State.Strict (evalStateT, get, gets, modify', state)
import Data.Char (isAlphaNum, isSpace)
import Data.Functor (($>))
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text as Text (
  Text,
  all,
  drop,
  isPrefixOf,
  length,
  lines,
  null,
  pack,
  replace,
  replicate,
  strip,
  stripEnd,
  stripPrefix,
  stripStart,
  take,
  takeWhile,
  takeWhileEnd,
  unwords,
 )
import Data.Void (Void)
import Nixfmt.Types (
  Ann (..),
  Directive (..),
  Parser,
  ParserState (..),
  TrailingComment (..),
  Trivia,
  Trivium (..),
  Whole (..),
 )
import Nixfmt.Util (isSpaces, manyP)
import Text.Megaparsec (
  Parsec,
  Pos,
  SourcePos (..),
  anySingle,
  chunk,
  getInput,
  getOffset,
  getSourcePos,
  hidden,
  lookAhead,
  many,
  manyTill,
  notFollowedBy,
  optional,
  some,
  try,
  unPos,
  (<|>),
 )
import Text.Megaparsec.Char (char, eol, hspace)

data ParseTrivium
  = PTNewlines Int
  | -- Track the column where the comment starts
    PTLineComment Text Pos
  | -- Track whether it is a doc comment
    PTBlockComment Bool [Text]
  | -- | Language annotation like /* lua */ (single line, non-doc)
    PTLanguageAnnotation Text
  | -- | /*nixfmt:disable*/ (True) or /*nixfmt:enable*/ (False), with its
    -- (start, end) input offsets; the end includes trailing whitespace on the line.
    PTFormatDirective Bool (Int, Int)
  deriving (Show)

-- | Horizontal whitespace: whitespace that does not end the line.
isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

preLexeme :: Parser a -> Parser a
preLexeme p = p <* manyP isHSpace

newlines :: Parser ParseTrivium
newlines = PTNewlines . Prelude.length <$> some (preLexeme eol)

-- | Parse a /*nixfmt:disable*/ or /*nixfmt:enable*/ directive with nothing
-- else following on its line.
formatDirective :: Parser ParseTrivium
formatDirective = try $ do
  start <- getOffset
  _ <- chunk "/*nixfmt:"
  isDisable <- (True <$ chunk "disable") <|> (False <$ chunk "enable")
  _ <- chunk "*/"
  rest <- manyP (\x -> x /= '\n' && x /= '\r')
  guard $ Text.all isHSpace rest
  end <- getOffset
  return (PTFormatDirective isDisable (start, end))

lineComment :: Parser ParseTrivium
lineComment = preLexeme $ do
  SourcePos{sourceColumn = col} <- getSourcePos
  _ <- chunk "#"
  text <- stripEnd <$> manyP (\x -> x /= '\n' && x /= '\r')
  return (PTLineComment text col)

blockComment :: Parser ParseTrivium
blockComment = try $ preLexeme $ do
  SourcePos{sourceColumn = pos} <- getSourcePos
  -- Positions start counting at 1, which we don't want here
  let pos' = unPos pos - 1
  _ <- chunk "/*"
  -- Try to parse /** before /*, but don't parse /**/ (i.e. the empty comment)
  isDoc <- try ((True <$ char '*') <* notFollowedBy (char '/')) <|> pure False

  chars <- manyTill anySingle $ chunk "*/"
  return
    . PTBlockComment isDoc
    . dropWhile Text.null
    . fixIndent pos'
    . dropWhileEnd Text.null
    . map Text.stripEnd
    . removeStars pos'
    . splitLines
    . pack
    $ chars
  where
    -- Normalize line ends and stuff
    splitLines :: Text -> [Text]
    splitLines = dropWhileEnd Text.null . map Text.stripEnd . Text.lines . replace "\r\n" "\n"

    -- If all lines (but the first) start with a star (and the star is at the correct position),
    -- replace that star with whitespace.
    removeStars :: Int -> [Text] -> [Text]
    removeStars _ [] = []
    removeStars pos (h : t) =
      -- Replace the * with whitespace. Only do so when all lines correctly match.
      -- The * must be aligned with the opening /*
      h : (fromMaybe t . traverse (fmap (newStart <>) . stripPrefix start) $ t)
      where
        start = Text.replicate pos " " <> " *"
        newStart = Text.replicate pos " "

    -- Strip the indented prefix of all lines
    -- If the first line is empty, we set the minimum indentation to +2.
    -- However, if there is a first line and it is aligned with the others, use +3 instead.
    fixIndent :: Int -> [Text] -> [Text]
    fixIndent _ [] = []
    fixIndent pos (h : t) =
      strip h : map (stripIndentation $ commonIndentationLength offset $ filter (not . isSpaces) t) t
      where
        offset = if " " `isPrefixOf` h then pos + 3 else pos + 2

    stripIndentation :: Int -> Text -> Text
    stripIndentation n t = fromMaybe (stripStart t) $ stripPrefix (Text.replicate n " ") t

    commonIndentationLength :: Int -> [Text] -> Int
    commonIndentationLength = foldr (min . Text.length . Text.takeWhile (== ' '))

languageAnnotation :: Parser ParseTrivium
languageAnnotation = try $ do
  -- Parse a block comment and extract its content
  PTBlockComment False [content] <- blockComment
  isStringDelimiterNext <- lookAhead isNextStringDelimiter

  if isStringDelimiterNext && isValidLanguageIdentifier content
    then return (PTLanguageAnnotation (strip content))
    else fail "Not a language annotation"
  where
    -- Check if a text is a valid language identifier for language annotations
    isValidLanguageIdentifier txt =
      let stripped = strip txt
      in not (Text.null stripped)
           && Text.length stripped <= 30
           && Text.all (\c -> isAlphaNum c || elem @[] c ['-', '+', '.', '_']) stripped

    -- Parser to peek at the next token to see if it's a string delimiter (" or '')
    isNextStringDelimiter = do
      hspace -- Skip horizontal whitespace (spaces/tabs)
      _ <- optional eol -- Optionally consume one newline
      hspace -- Skip more horizontal whitespace
      (chunk "\"" $> True)
        <|> (chunk "''" $> True)
        <|> pure False

-- This should be called with zero or one elements, as per `span isTrailing`
convertTrailing :: [ParseTrivium] -> Maybe TrailingComment
convertTrailing = toMaybe . join . map toText
  where
    toText (PTLineComment c _) = strip c
    toText (PTBlockComment False [c]) = strip c
    toText (PTFormatDirective True _) = "nixfmt:disable"
    toText (PTFormatDirective False _) = "nixfmt:enable"
    toText _ = ""
    join = Text.unwords . filter (/= "")
    toMaybe "" = Nothing
    toMaybe c = Just $ TrailingComment c

-- | Resolve a format directive against the currently open disabled region.
-- Directives sharing their line with other tokens are demoted to plain
-- comments; closing a region captures its raw source text (whole lines,
-- both directives included).
resolveDirective :: Bool -> (Int, Int) -> Parser Trivium
resolveDirective isDisable (start, end) = do
  ParserState{source, pendingDisable} <- get
  let linePrefix = Text.takeWhileEnd (/= '\n') (Text.take start source)
  if not (Text.all isHSpace linePrefix)
    then pure $ LineComment $ " nixfmt:" <> (if isDisable then "disable" else "enable")
    else do
      let lineStart = start - Text.length linePrefix
      fmap FormatDirective $ case (isDisable, pendingDisable) of
        (True, Nothing) -> Disable <$ modify' (\s -> s{pendingDisable = Just lineStart})
        -- A disable inside an already disabled region is inert
        (True, Just _) -> pure Disable
        (False, Just regionStart) ->
          Enable (Just (sliceSource regionStart end source))
            <$ modify' (\s -> s{pendingDisable = Nothing})
        (False, Nothing) -> pure $ Enable Nothing

sliceSource :: Int -> Int -> Text -> Text
sliceSource start end = Text.take (end - start) . Text.drop start

convertLeadingM :: [ParseTrivium] -> Parser Trivia
convertLeadingM = fmap mconcat . traverse convert
  where
    convert :: ParseTrivium -> Parser Trivia
    convert = \case
      PTNewlines 1 -> pure []
      PTNewlines _ -> pure [EmptyLine]
      PTLineComment c _ -> pure [LineComment c]
      PTBlockComment _ [] -> pure []
      PTBlockComment False [c] -> pure [LineComment $ " " <> strip c]
      PTBlockComment isDoc cs -> pure [BlockComment isDoc cs]
      PTLanguageAnnotation c -> pure [LanguageAnnotation c]
      PTFormatDirective isDisable offsets -> pure <$> resolveDirective isDisable offsets

isTrailing :: ParseTrivium -> Bool
isTrailing (PTLineComment _ _) = True
isTrailing (PTBlockComment False []) = True
isTrailing (PTBlockComment False [_]) = True
isTrailing (PTFormatDirective _ _) = True
isTrailing _ = False

convertTrivia :: [ParseTrivium] -> Pos -> Parser (Maybe TrailingComment, Trivia)
convertTrivia pts nextCol =
  let (trailing, leading) = span isTrailing pts
  in case (trailing, leading) of
       -- Special case: if the trailing comment visually forms a block with the start of the following line,
       -- then treat it like part of those comments instead of a distinct trailing comment.
       -- This happens especially often after `{` or `[` tokens, where the comment of the first item
       -- starts on the same line ase the opening token.
       ([PTLineComment _ pos], (PTNewlines 1) : (PTLineComment _ pos') : _) | pos == pos' -> (,) Nothing <$> convertLeadingM pts
       ([PTLineComment _ pos], [PTNewlines 1]) | pos == nextCol -> (,) Nothing <$> convertLeadingM pts
       _ -> (,) (convertTrailing trailing) <$> convertLeadingM leading

trivia :: Parser [ParseTrivium]
trivia = many $ hidden $ languageAnnotation <|> formatDirective <|> lineComment <|> blockComment <|> newlines

-- The following primitives to interact with the state monad that stores trivia
-- are designed to prevent trivia from being dropped or duplicated by accident.

takeTrivia :: Parser Trivia
takeTrivia = state $ \s -> (pendingTrivia s, s{pendingTrivia = []})

pushTrivia :: Trivia -> Parser ()
pushTrivia t = modify' (\s -> s{pendingTrivia = pendingTrivia s <> t})

lexeme :: Parser a -> Parser (Ann a)
lexeme p = do
  lastLeading <- takeTrivia
  SourcePos{Text.Megaparsec.sourceLine = line} <- getSourcePos
  token <- preLexeme p
  parsedTrivia <- trivia
  -- This is the position of the next lexeme after the currently parsed one
  SourcePos{sourceColumn = col} <- getSourcePos
  (trailing, nextLeading) <- convertTrivia parsedTrivia col
  pushTrivia nextLeading
  pure $!
    Ann
      { preTrivia = lastLeading,
        value = token,
        Nixfmt.Types.sourceLine = line,
        trailComment = trailing
      }

-- | Tokens normally have only leading trivia and one trailing comment on the same
-- line. A whole x also parses and stores final trivia after the x. A whole also
-- does not interact with the trivia state of its surroundings.
--
-- The top-level entry point: initializes the parser state and closes any
-- disabled region left unclosed at the end of the file.
whole :: Parser a -> Parsec Void Text (Whole a)
whole pa = do
  src <- getInput
  let initialState =
        ParserState
          { pendingTrivia = [],
            source = src,
            pendingDisable = Nothing,
            cutInterpolations = 0
          }
  evalStateT (closeUnclosedRegion =<< wholeInner pa) initialState

-- | An unclosed /*nixfmt:disable*/ extends to the end of the file.
closeUnclosedRegion :: Whole a -> Parser (Whole a)
closeUnclosedRegion (Whole a finalTrivia) = do
  ParserState{source, pendingDisable} <- get
  pure $ case pendingDisable of
    Nothing -> Whole a finalTrivia
    Just regionStart -> Whole a $ finalTrivia <> [FormatDirective $ DisabledToEof (Text.drop regionStart source)]

-- | Like 'whole', but running within an enclosing parse (used for
-- interpolations): isolates the surrounding pending trivia while the
-- directive state keeps flowing through in source order.
wholeInner :: Parser a -> Parser (Whole a)
wholeInner pa = do
  saved <- takeTrivia
  ParserState{pendingDisable = before, cutInterpolations = outerCuts} <- get
  preLexeme $ pure ()
  pushTrivia =<< convertLeadingM =<< trivia
  result <- Whole <$> pa <*> takeTrivia
  after <- gets pendingDisable
  -- A disabled region with exactly one end inside this interpolation cuts it.
  -- Set rather than incremented: cuts of interpolations nested deeper inside
  -- were already observed by their enclosing string and must not leak out.
  modify' $ \s -> s{cutInterpolations = outerCuts + (if before == after then 0 else 1)}
  pushTrivia saved
  pure result
