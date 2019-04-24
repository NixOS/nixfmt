{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Nixfmt.Lexer
    ( lexeme
    ) where

import Data.Char
import Data.Text as Text (Text, cons, intercalate, lines, pack, replace, strip)
import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec.Char

import Nixfmt.Types (Ann(..), Parser, Trivia, Trivium(..))
import Nixfmt.Util

data ParseTrivium
    = PTNewlines     Int
    | PTLineComment  Text
    | PTBlockComment [Text]
    deriving (Show)

preLexeme :: Parser a -> Parser a
preLexeme p = p <* manyP (\x -> isSpace x && x /= '\n' && x /= '\r')

newlines :: Parser ParseTrivium
newlines = PTNewlines <$> length <$> some (preLexeme eol)

splitLines :: Text -> [Text]
splitLines = Text.lines . replace "\r\n" "\n"

lineComment :: Parser ParseTrivium
lineComment = preLexeme $ chunk "#" *>
    (PTLineComment <$> manyP (\x -> x /= '\n' && x /= '\r'))

blockComment :: Parser ParseTrivium
blockComment = try $ preLexeme $ chunk "/*" *>
    (PTBlockComment . splitLines . pack <$> manyTill anySingle (chunk "*/"))

convertTrailing :: [ParseTrivium] -> Maybe Text
convertTrailing = toMaybe . join . map toText
    where toText (PTLineComment c)    = strip c
          toText (PTBlockComment [c]) = strip c
          toText _                    = ""
          join = intercalate " " . filter (/="")
          toMaybe "" = Nothing
          toMaybe c  = Just (cons ' ' c)


convertLeading :: [ParseTrivium] -> Trivia
convertLeading = concatMap (\case
    (PTNewlines 1) -> []
    (PTNewlines _) -> [EmptyLine]
    (PTLineComment lc) -> [LineComment lc]
    (PTBlockComment bc) -> [BlockComment $ dropCommonIndentation bc])

isTrailing :: ParseTrivium -> Bool
isTrailing (PTLineComment _)    = True
isTrailing (PTBlockComment [_]) = True
isTrailing _                    = False

convertTrivia :: [ParseTrivium] -> (Maybe Text, Trivia)
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
