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
import           Text.Megaparsec            hiding (optional, sepBy)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

optional :: Parser [a] -> Parser [a]
optional = option []

sepBy :: NixToken -> Parser [NixAST] -> Parser [NixAST]
sepBy sep p = optional $ p <> manyList (symbol sep <> p)

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

nixSearchPath :: Parser NixToken
nixSearchPath = try $ EnvPath <$> (char '<' *>
    liftM2 Text.append
        (someP pathChar)
        (manyText $ liftM2 Text.cons (char '/') (someP pathChar))
    <* char '>')

nixPath :: Parser NixToken
nixPath = try $ NixURI <$> liftM2 Text.append
    (manyP pathChar)
    (someText $ liftM2 Text.cons (char '/') (someP pathChar))

nixInt :: Parser NixToken
nixInt = try $ NixInt <$> decimal

brackets :: Parser [NixAST] -> Parser [NixAST]
brackets p = symbol TBrackOpen <> p <> symbol TBrackClose

braces :: Parser [NixAST] -> Parser [NixAST]
braces p = symbol TBraceOpen <> p <> symbol TBraceClose

parens :: Parser [NixAST] -> Parser [NixAST]
parens p = symbol TParenOpen <> p <> symbol TParenClose

fieldParameter :: Parser [NixAST]
fieldParameter = node FieldParameter $
    identifier <> optional (symbol TQuestion <> nixTerm)

setParameter :: Parser [NixAST]
setParameter = try $ node SetParameter $ braces $
    sepBy TComma fieldParameter <> optional (symbol TComma <> symbol TEllipsis)

contextParameter :: Parser [NixAST]
contextParameter = node ContextParameter $
    try (setParameter <> symbol TAt <> identifier) <|>
    try (identifier <> symbol TAt <> setParameter)

abstraction :: Parser [NixAST]
abstraction = try $
    (contextParameter <|> setParameter <|> identifier) <>
    symbol TColon <> nixTerm

nixInherit :: Parser [NixAST]
nixInherit = node Inherit $
    reserved TInherit <> optional (parens identifier) <> manyList identifier <>
    symbol TSemicolon

assignment :: Parser [NixAST]
assignment = node Assignment $
    identifier <> symbol TAssign <> nixTerm <> symbol TSemicolon

nixWith :: Parser [NixAST]
nixWith = try $ node With $
    reserved TWith <> nixTerm <> symbol TSemicolon <> nixTerm

nixLet :: Parser [NixAST]
nixLet = try $ node Let $
    reserved TLet <> manyList assignment <> reserved TIn <> nixTerm

nixSet :: Parser [NixAST]
nixSet = try $ node Set $ braces $ manyList assignment

nixValue :: Parser [NixAST]
nixValue = lexeme (nixSearchPath <|> nixPath <|> nixInt)

nixTerm :: Parser [NixAST]
nixTerm = abstraction <|> nixWith <|> nixLet <|>
    nixSet <|> nixValue <|> identifier <|> nixList

nixList :: Parser [NixAST]
nixList = try $ node List $ brackets $ manyList nixTerm

nixFile :: Parser NixAST
nixFile = file nixTerm
