{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE BlockArguments, FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Nixfmt.Lexer (lexeme, pushTrivia, takeTrivia, whole) where

import Control.Monad.State (MonadState, evalStateT, get, modify, put)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text as Text
  (Text, length, lines, null, pack, replace, replicate, strip, stripEnd,
  stripPrefix, stripStart, takeWhile, unwords)
import Data.Void (Void)
import Text.Megaparsec
  (Parsec, SourcePos(..), Pos, anySingle, chunk, getSourcePos, hidden, many,
  manyTill, some, try, unPos, (<|>))
import Text.Megaparsec.Char (eol)

import Nixfmt.Types
  (Ann(..), Whole(..), Parser, TrailingComment(..), Trivia, Trivium(..))
import Nixfmt.Util (manyP)

data ParseTrivium
    = PTNewlines     Int
      -- Track the column where the comment starts
    | PTLineComment  Text Pos
    | PTBlockComment [Text]
    deriving (Show)

preLexeme :: Parser a -> Parser a
preLexeme p = p <* manyP (\x -> isSpace x && x /= '\n' && x /= '\r')

newlines :: Parser ParseTrivium
newlines = PTNewlines . Prelude.length <$> some (preLexeme eol)

splitLines :: Text -> [Text]
splitLines = dropWhile Text.null . dropWhileEnd Text.null
    . map Text.stripEnd . Text.lines . replace "\r\n" "\n"

stripIndentation :: Int -> Text -> Text
stripIndentation n t = fromMaybe (stripStart t) $ stripPrefix (Text.replicate n " ") t

commonIndentationLength :: Int -> [Text] -> Int
commonIndentationLength def = foldr min def . map (Text.length . Text.takeWhile (==' '))

fixLines :: Int -> [Text] -> [Text]
fixLines _ []      = []
fixLines n (h : t) = strip h
    : map (stripIndentation $ commonIndentationLength n $ filter (/="") t) t

lineComment :: Parser ParseTrivium
lineComment = preLexeme $ do
    SourcePos{sourceColumn = col} <- getSourcePos
    _ <- chunk "#"
    text <- manyP (\x -> x /= '\n' && x /= '\r')
    return (PTLineComment text col)

blockComment :: Parser ParseTrivium
blockComment = try $ preLexeme $ do
    _ <- chunk "/*"
    SourcePos{sourceColumn = pos} <- getSourcePos
    chars <- manyTill anySingle $ chunk "*/"
    return $ PTBlockComment $ fixLines (unPos pos) $ splitLines $ pack chars

-- This should be called with zero or one elements, as per `span isTrailing`
convertTrailing :: [ParseTrivium] -> Maybe TrailingComment
convertTrailing = toMaybe . join . map toText
    where toText (PTLineComment c _)    = strip c
          toText (PTBlockComment [c]) = strip c
          toText _                    = ""
          join = Text.unwords . filter (/="")
          toMaybe "" = Nothing
          toMaybe c  = Just $ TrailingComment c

convertLeading :: [ParseTrivium] -> Trivia
convertLeading = concatMap (\case
    PTNewlines 1       -> []
    PTNewlines _       -> [EmptyLine]
    PTLineComment c _  -> [LineComment c]
    PTBlockComment []  -> []
    PTBlockComment [c] -> [LineComment $ " " <> strip c]
    PTBlockComment cs  -> [BlockComment cs])

isTrailing :: ParseTrivium -> Bool
isTrailing (PTLineComment _ _)  = True
isTrailing (PTBlockComment [])  = True
isTrailing (PTBlockComment [_]) = True
isTrailing _                    = False

convertTrivia :: [ParseTrivium] -> (Maybe TrailingComment, Trivia)
convertTrivia pts =
    let (trailing, leading) = span isTrailing pts
    in case (trailing, leading) of
        -- Special case: if the trailing comment visually forms a block with the following line comments,
        -- then treat it like part of those comments instead of a distinct trailing comment.
        -- This happens especially often after `{` or `[` tokens, where the comment of the first item
        -- starts on the same line ase the opening token.
        ([PTLineComment _ pos], (PTNewlines 1):(PTLineComment _ pos'):_) | pos == pos' -> (Nothing, convertLeading pts)
        _ -> (convertTrailing trailing, convertLeading leading)

trivia :: Parser [ParseTrivium]
trivia = many $ hidden $ lineComment <|> blockComment <|> newlines

-- The following primitives to interact with the state monad that stores trivia
-- are designed to prevent trivia from being dropped or duplicated by accident.

takeTrivia :: MonadState Trivia m => m Trivia
takeTrivia = get <* put []

pushTrivia :: MonadState Trivia m => Trivia -> m ()
pushTrivia t = modify (<>t)

lexeme :: Parser a -> Parser (Ann a)
lexeme p = do
    lastLeading <- takeTrivia
    token <- preLexeme p
    (trailing, nextLeading) <- convertTrivia <$> trivia
    pushTrivia nextLeading
    return $ Ann lastLeading token trailing

-- | Tokens normally have only leading trivia and one trailing comment on the same
-- line. A whole x also parses and stores final trivia after the x. A whole also
-- does not interact with the trivia state of its surroundings.
whole :: Parser a -> Parsec Void Text (Whole a)
whole pa = flip evalStateT [] do
    preLexeme $ pure ()
    pushTrivia . convertLeading =<< trivia
    Whole <$> pa <*> takeTrivia
