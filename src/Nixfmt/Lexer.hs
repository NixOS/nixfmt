{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Nixfmt.Lexer (lexeme) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text as Text
  (Text, intercalate, length, lines, null, pack, replace, replicate, strip,
  stripEnd, stripPrefix, stripStart, takeWhile)
import Text.Megaparsec
  (SourcePos(..), anySingle, chunk, getSourcePos, hidden, many, manyTill, some,
  try, unPos, (<|>))
import Text.Megaparsec.Char (eol)

import Nixfmt.Types (Ann(..), Parser, TrailingComment(..), Trivia, Trivium(..))
import Nixfmt.Util (manyP)

data ParseTrivium
    = PTNewlines     Int
    | PTLineComment  Text
    | PTBlockComment [Text]
    deriving (Show)

preLexeme :: Parser a -> Parser a
preLexeme p = p <* manyP (\x -> isSpace x && x /= '\n' && x /= '\r')

newlines :: Parser ParseTrivium
newlines = PTNewlines <$> Prelude.length <$> some (preLexeme eol)

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
lineComment = preLexeme $ chunk "#" *>
    (PTLineComment <$> manyP (\x -> x /= '\n' && x /= '\r'))

blockComment :: Parser ParseTrivium
blockComment = try $ preLexeme $ do
    _ <- chunk "/*"
    SourcePos{sourceColumn = pos} <- getSourcePos
    chars <- manyTill anySingle $ chunk "*/"
    return $ PTBlockComment $ fixLines (unPos pos) $ splitLines $ pack chars

convertTrailing :: [ParseTrivium] -> Maybe TrailingComment
convertTrailing = toMaybe . join . map toText
    where toText (PTLineComment c)    = strip c
          toText (PTBlockComment [c]) = strip c
          toText _                    = ""
          join = intercalate " " . filter (/="")
          toMaybe "" = Nothing
          toMaybe c  = Just $ TrailingComment c

convertLeading :: [ParseTrivium] -> Trivia
convertLeading = concatMap (\case
    PTNewlines 1       -> []
    PTNewlines _       -> [EmptyLine]
    PTLineComment c    -> [LineComment c]
    PTBlockComment []  -> []
    PTBlockComment [c] -> [LineComment $ " " <> strip c]
    PTBlockComment cs  -> [BlockComment cs])

isTrailing :: ParseTrivium -> Bool
isTrailing (PTLineComment _)    = True
isTrailing (PTBlockComment [])  = True
isTrailing (PTBlockComment [_]) = True
isTrailing _                    = False

convertTrivia :: [ParseTrivium] -> (Maybe TrailingComment, Trivia)
convertTrivia pts =
    let (trailing, leading) = span isTrailing pts
    in (convertTrailing trailing, convertLeading leading)

trivia :: Parser [ParseTrivium]
trivia = many $ hidden $ lineComment <|> blockComment <|> newlines

lexeme :: Parser a -> Parser (Ann a)
lexeme p = do
    token <- preLexeme p
    (trailing, leading) <- convertTrivia <$> trivia
    return $ Ann token trailing leading
