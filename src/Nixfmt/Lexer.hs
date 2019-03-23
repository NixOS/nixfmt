{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Lexer
    ( trivia
    , lexeme
    , symbol
    ) where

import           Data.Char
import           Data.Text            as Text hiding (filter, length, map, span)
import           Nixfmt.Types
import           Nixfmt.Util
import           Text.Megaparsec
import           Text.Megaparsec.Char

data ParseTrivium = PTNewlines     Int
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
lineComment = preLexeme $ string "#" *>
    (PTLineComment <$> manyP (\x -> x /= '\n' && x /= '\r'))

blockComment :: Parser ParseTrivium
blockComment = try $ preLexeme $ string "/*" *>
    (PTBlockComment . splitLines . pack <$> manyTill anySingle (string "*/"))

convertTrailing :: [ParseTrivium] -> [Trivium]
convertTrailing = (\case
        "" -> []
        tc -> [TrailingComment $ cons ' ' tc]
    ) . intercalate " " . filter (/="") . map (\case
        (PTLineComment lc) -> strip lc
        (PTBlockComment [bc]) -> strip bc
        _ -> "")

convertLeading :: [ParseTrivium] -> Trivia
convertLeading = Prelude.concat . map (\case
    (PTNewlines 1) -> []
    (PTNewlines _) -> [EmptyLine]
    (PTLineComment lc) -> [LineComment lc]
    (PTBlockComment bc) -> [BlockComment $ dropCommonIndentation bc])

isTrailing :: ParseTrivium -> Bool
isTrailing (PTLineComment _)    = True
isTrailing (PTBlockComment [_]) = True
isTrailing _                    = False

convertTrivia :: [ParseTrivium] -> Trivia
convertTrivia pts = let (trailing, leading) = span isTrailing pts
                    in convertTrailing trailing ++ convertLeading leading

trivia :: Parser [NixAST]
trivia = pure . Leaf . Trivia . convertTrivia
    <$> many (lineComment <|> blockComment <|> newlines)

lexeme :: Parser NixToken -> Parser [NixAST]
lexeme p = preLexeme (pure . Leaf <$> p) <> trivia

symbol :: NixToken -> Parser [NixAST]
symbol t = lexeme (string (pack $ show t) *> return t)
