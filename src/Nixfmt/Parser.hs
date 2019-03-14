{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nixfmt.Parser
    ( nixFile
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text                  as Text hiding (length)
import           Data.Void
import           Nixfmt.Types
import           Text.Megaparsec            hiding (between)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

someP :: (Char -> Bool) -> Parser Text
someP = takeWhile1P Nothing

manyP :: (Char -> Bool) -> Parser Text
manyP = takeWhileP Nothing

someCat :: Parser Text -> Parser Text
someCat p = Text.concat <$> some p

manyCat :: Parser Text -> Parser Text
manyCat p = Text.concat <$> many p

between :: Parser a -> Parser a -> Parser [a] -> Parser [a]
between preParser postParser bodyParser = do
    pre <- preParser
    body <- bodyParser
    post <- postParser
    return $ [pre] ++ body ++ [post]

lineComment :: Parser Trivium
lineComment = string "#" *>
    (LineComment <$> takeWhileP Nothing (\x -> x /= '\n' && x /= '\r'))

blockComment :: Parser Trivium
blockComment = string "/*" *>
    (BlockComment <$> pack <$> manyTill anySingle (string "*/"))

newlines :: Parser Trivium
newlines = Newlines <$> length <$> some eol

tabs :: Parser Trivium
tabs = Tabs <$> length <$> some tab

spaces :: Parser Trivium
spaces = Spaces <$> length <$> some (char ' ')

trailingTrivia :: Parser [Trivium]
trailingTrivia = many (lineComment <|> blockComment <|> tabs <|> spaces)

leadingTrivia :: Parser [Trivium]
leadingTrivia = many (lineComment <|> blockComment <|> tabs <|> spaces <|> newlines)

lexeme :: Parser a -> Parser (Ann a)
lexeme p = do
    preTrivia <- leadingTrivia
    startPos <- getSourcePos
    content <- p
    endPos <- getSourcePos
    postTrivia <- trailingTrivia
    return $ Ann preTrivia (Just startPos) content (Just endPos) postTrivia

symbol :: NixToken -> Parser AST
symbol t = Leaf <$> lexeme (string (pack $ show t) *> return t)

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

identifier :: Parser AST
identifier = liftM Leaf $ try $ lexeme $ Identifier <$> do
    ident <- Text.cons <$> satisfy (\x -> isAlpha x || x == '_')
                       <*> manyP identChar
    guard $ not $ ident `elem` reservedNames
    return ident

reserved :: NixToken -> Parser AST
reserved t = try $ Leaf <$> lexeme (string (pack $ show t)
    *> lookAhead (satisfy (\x -> not (identChar x) && not (pathChar x)))
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
    , manyCat $ liftM2 Text.cons (char '/') (someP pathChar)
    , singleton <$> char '>'
    ]

nixPath :: Parser NixToken
nixPath = try $ Literal <$> NixURI <$> liftM2 Text.append
    (manyP pathChar)
    (someCat $ liftM2 Text.cons (char '/') (someP pathChar))

nixInt :: Parser NixToken
nixInt = try $ Literal <$> NixInt <$> decimal

nixValue :: Parser AST
nixValue = Leaf <$> lexeme (nixSearchPath <|> nixPath <|> nixInt)

nixTerm :: Parser AST
nixTerm = nixValue <|> identifier <|> try nixList

brackets :: Parser [AST] -> Parser [AST]
brackets = between (symbol TBrackOpen) (symbol TBrackClose)

nixList :: Parser AST
nixList = Node List <$> (brackets $ many nixTerm)

nixFile :: Parser AST
nixFile = do
    term <- nixTerm
    eofToken <- Leaf <$> lexeme (eof *> return TEOF)
    return $ Node File $ [term, eofToken]
