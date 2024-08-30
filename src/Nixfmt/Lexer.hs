module Nixfmt.Lexer (lexeme, pushTrivia, takeTrivia, whole) where

import Control.DeepSeq (NFData, force)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify, put)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, singleton)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text as Text (
  Text,
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
  takeWhile,
  unwords,
 )
import Data.Void (Void)
import GHC.Generics (Generic)
import Nixfmt.Types (
  Ann (..),
  Parser,
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
  getSourcePos,
  hidden,
  many,
  manyTill,
  notFollowedBy,
  some,
  try,
  unPos,
  (<|>),
 )
import Text.Megaparsec.Char (char, eol)

data ParseTrivium
  = PTNewlines {-# UNPACK #-} !Int
  | -- Track the column where the comment starts
    PTLineComment {-# UNPACK #-} !Text !Pos
  | -- Track whether it is a doc comment
    PTBlockComment !Bool ![Text]
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

preLexeme :: Parser a -> Parser a
preLexeme p = p <* manyP (\x -> isSpace x && x /= '\n' && x /= '\r')

newlines :: Parser ParseTrivium
newlines = PTNewlines . Prelude.length <$> some (preLexeme eol)

lineComment :: Parser ParseTrivium
lineComment = preLexeme $ do
  SourcePos{sourceColumn = col} <- getSourcePos
  _ <- chunk "#"
  text <- manyP (\x -> x /= '\n' && x /= '\r')
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

-- This should be called with zero or one elements, as per `span isTrailing`
convertTrailing :: Seq ParseTrivium -> Maybe TrailingComment
convertTrailing = toMaybe . join . foldMap (singleton . toText)
  where
    toText (PTLineComment c _) = strip c
    toText (PTBlockComment False [c]) = strip c
    toText _ = ""
    join = Text.unwords . filter (/= "")
    toMaybe "" = Nothing
    toMaybe c = Just $ TrailingComment c

convertLeading :: Seq ParseTrivium -> Trivia
convertLeading =
  foldMap
    ( \case
        PTNewlines 1 -> []
        PTNewlines _ -> [EmptyLine]
        PTLineComment c _ -> [LineComment c]
        PTBlockComment _ [] -> []
        PTBlockComment False [c] -> [LineComment $ " " <> strip c]
        PTBlockComment isDoc cs -> [BlockComment isDoc cs]
    )

isTrailing :: ParseTrivium -> Bool
isTrailing (PTLineComment _ _) = True
isTrailing (PTBlockComment False []) = True
isTrailing (PTBlockComment False [_]) = True
isTrailing _ = False

convertTrivia :: Seq ParseTrivium -> Pos -> (Maybe TrailingComment, Trivia)
convertTrivia pts nextCol =
  let (trailing, leading) = Seq.spanl isTrailing pts
  in case (trailing, leading) of
      -- Special case: if the trailing comment visually forms a block with the start of the following line,
      -- then treat it like part of those comments instead of a distinct trailing comment.
      -- This happens especially often after `{` or `[` tokens, where the comment of the first item
      -- starts on the same line ase the opening token.
      ([PTLineComment _ pos], (PTNewlines 1) Seq.:<| (PTLineComment _ pos') Seq.:<| _) | pos == pos' -> (Nothing, convertLeading pts)
      ([PTLineComment _ pos], [PTNewlines 1]) | pos == nextCol -> (Nothing, convertLeading pts)
      _ -> (convertTrailing trailing, convertLeading leading)

trivia :: Parser (Seq ParseTrivium)
trivia =
  Seq.fromList <$> do
    many $ hidden $ lineComment <|> blockComment <|> newlines

-- The following primitives to interact with the state monad that stores trivia
-- are designed to prevent trivia from being dropped or duplicated by accident.

takeTrivia :: (Monad m) => StateT Trivia m Trivia
takeTrivia = get <* put Seq.empty

pushTrivia :: (Monad m) => Trivia -> StateT Trivia m ()
pushTrivia t = modify (<> t)
{-# INLINEABLE pushTrivia #-}

lexeme :: (NFData a) => Parser a -> Parser (Ann a)
lexeme p = do
  lastLeading <- takeTrivia
  SourcePos{Text.Megaparsec.sourceLine = line} <- getSourcePos
  token <- preLexeme p
  parsedTrivia <- trivia
  -- This is the position of the next lexeme after the currently parsed one
  SourcePos{sourceColumn = col} <- getSourcePos
  let (trailing, nextLeading) = convertTrivia parsedTrivia col
  pushTrivia nextLeading
  return $
    Ann
      { preTrivia = force lastLeading,
        value = force token,
        Nixfmt.Types.sourceLine = force line,
        trailComment = force trailing
      }

-- | Tokens normally have only leading trivia and one trailing comment on the same
-- line. A whole x also parses and stores final trivia after the x. A whole also
-- does not interact with the trivia state of its surroundings.
whole :: (NFData a) => Parser a -> Parsec Void Text (Whole a)
whole pa = flip evalStateT Seq.empty do
  preLexeme $ pure ()
  trivia
    >>= pushTrivia . convertLeading
  Whole <$> (force <$> pa) <*> (force <$> takeTrivia)
