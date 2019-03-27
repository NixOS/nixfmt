{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Parser
    ( nixFile
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text                  as Text hiding (length, map, tail)
import           Nixfmt.Lexer
import           Nixfmt.Types
import           Nixfmt.Util
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

node :: NodeType -> Parser [NixAST] -> Parser [NixAST]
node nodeType p = pure . Node nodeType <$> p

reservedNames :: [Text]
reservedNames =
    [ "let", "in"
    , "if", "then", "else"
    , "assert"
    , "with"
    , "rec"
    , "inherit"
    ]

identChar :: Char -> Bool
identChar x = isAlpha x || isDigit x || x == '_' || x == '\'' || x == '-'

identifier :: Parser [NixAST]
identifier = try $ lexeme $ Identifier <$> do
    ident <- Text.cons <$> satisfy (\x -> isAlpha x || x == '_')
                       <*> manyP identChar
    guard $ not $ ident `elem` reservedNames
    return ident

reserved :: NixToken -> Parser [NixAST]
reserved t = try $ lexeme (string (pack $ show t)
    *> lookAhead (satisfy (\x -> not $ identChar x || pathChar x))
    *> return t)

pathChar :: Char -> Bool
pathChar x = isAlpha x || isDigit x
    || x == '.' || x == '_' || x == '-' || x == '+' || x == '~'

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSearchPath :: Parser NixToken
nixSearchPath = try $ EnvPath <$> Text.concat <$> sequence
    [ singleton <$> char '<'
    , someP pathChar
    , manyText $ liftM2 Text.cons (char '/') (someP pathChar)
    , singleton <$> char '>'
    ]

nixPath :: Parser NixToken
nixPath = try $ NixURI <$> liftM2 Text.append
    (manyP pathChar)
    (someText $ liftM2 Text.cons (char '/') (someP pathChar))

nixInt :: Parser NixToken
nixInt = try $ NixInt <$> decimal

nixValue :: Parser [NixAST]
nixValue = lexeme (nixSearchPath <|> nixPath <|> nixInt)

nixTerm :: Parser [NixAST]
nixTerm = nixValue <|> identifier <|> try nixList

brackets :: Parser [NixAST] -> Parser [NixAST]
brackets p = (symbol TBrackOpen) <> p <> (symbol TBrackClose)

nixList :: Parser [NixAST]
nixList = node List $ brackets $ manyList nixTerm

nixFile :: Parser NixAST
nixFile = file nixTerm
