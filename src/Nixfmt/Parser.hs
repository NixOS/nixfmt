{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nixfmt.Parser where

import           Control.Monad
import           Data.Char
import           Data.Text            as Text hiding (length)
import           Data.Void
import           Nixfmt.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char

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
between pre post body = do
    pre <- pre
    body <- body
    post <- post
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

symbol :: NixToken -> Parser (Ann AST)
symbol t = lexeme $ string (pack $ show t) *> return (Leaf t)

identChar :: Char -> Bool
identChar x = isAlpha x || isDigit x || x == '_' || x == '\'' || x == '-'

identifier = lexeme $ try $ do
    ident <- Text.cons <$> satisfy (\x -> isAlpha x || x == '_')
                 <*> manyP identChar
    guard $ not $ ident `elem` reservedNames
    return ident

reserved :: NixToken -> Parser NixToken
reserved t = try $ (string (show t)
    *> lookAhead $ satisfy (\x -> not (identChar x) && not (pathChar x))
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

nixValue :: Parser AST
nixValue = Leaf <$> (nixSearchPath <|> nixPath)

