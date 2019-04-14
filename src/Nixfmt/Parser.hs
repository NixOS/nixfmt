{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Parser where

import           Control.Monad
import qualified Control.Monad.Combinators.Expr as MPExpr
import           Data.Char
import           Data.Text                      as Text hiding (length, map,
                                                         tail)
import           Nixfmt.Lexer
import           Nixfmt.Types
import           Nixfmt.Util
import           Text.Megaparsec                hiding (optional, sepBy)
import           Text.Megaparsec.Char           hiding (string)
import           Text.Megaparsec.Char.Lexer     (decimal)

optional :: Parser [a] -> Parser [a]
optional = option []

sepBy :: NixToken -> Parser [NixAST] -> Parser [NixAST]
sepBy sep p = optional $ p <> manyList (try $ symbol sep <> p)

node :: NodeType -> Parser [NixAST] -> Parser [NixAST]
node nodeType p = pure . Node nodeType <$> p

-- | parses a token without parsing trivia after it
rawSymbol :: NixToken -> Parser [NixAST]
rawSymbol t = chunk (pack $ show t) *> return [Leaf t Nothing]

reservedNames :: [Text]
reservedNames =
    [ "let", "in"
    , "if", "then", "else"
    , "assert"
    , "with"
    , "or"
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
reserved t = try $ lexeme (chunk (pack $ show t)
    *> lookAhead (satisfy (\x -> not $ identChar x || pathChar x))
    *> return t)

pathChar :: Char -> Bool
pathChar x = isAlpha x || isDigit x
    || x == '.' || x == '_' || x == '-' || x == '+' || x == '~'

slash :: Parser Text
slash = chunk "/" <* notFollowedBy (char '/')

nixSearchPath :: Parser NixToken
nixSearchPath = try $ EnvPath <$> (char '<' *>
    liftM2 Text.append
        (someP pathChar)
        (manyText $ slash <> someP pathChar)
    <* char '>')

nixPath :: Parser NixToken
nixPath = try $ NixURI <$> liftM2 Text.append
    (manyP pathChar)
    (someText $ slash <> someP pathChar)

nixInt :: Parser NixToken
nixInt = try $ NixInt <$> decimal

stringLeaf :: Parser Text -> Parser [NixAST]
stringLeaf p = do
    parsed <- p
    return [Leaf (NixText parsed) Nothing]

interpolation :: Parser [NixAST]
interpolation = node Interpolation $
    symbol TInterOpen <> expression <> rawSymbol TInterClose

simpleStringPart :: Parser [NixAST]
simpleStringPart = stringLeaf $ someText $
    chunk "\\n" *> "\n" <|>
    chunk "\\r" *> "\r" <|>
    chunk "\\t" *> "\t" <|>
    chunk "\\" *> (Text.singleton <$> anySingle) <|>
    chunk "$$" <> manyP (=='$') <|>
    try (chunk "$" <* notFollowedBy (char '{')) <|>
    someP (\t -> t /= '"' && t /= '\\' && t /= '$')

indentedStringPart :: Parser [NixAST]
indentedStringPart = stringLeaf $ someText $
    chunk "''\\n" *> "\n" <|>
    chunk "''\\r" *> "\r" <|>
    chunk "''\\t" *> "\t" <|>
    chunk "''\\" *> (Text.singleton <$> anySingle) <|>
    chunk "''$" *> "$" <|>
    chunk "'''" *> "''" <|>
    chunk "$$" <> manyP (=='$') <|>
    try (chunk "$" <* notFollowedBy (char '{')) <|>
    try (chunk "'" <* notFollowedBy (char '\'')) <|>
    someP (\t -> t /= '\'' && t /= '$')

simpleString :: Parser [NixAST]
simpleString = node SimpleString $ rawSymbol TDoubleQuote <>
    manyList (simpleStringPart <|> interpolation) <>
    symbol TDoubleQuote

indentedString :: Parser [NixAST]
indentedString = node IndentedString $ rawSymbol TDoubleSingleQuote <>
    manyList (indentedStringPart <|> interpolation) <>
    symbol TDoubleSingleQuote

string :: Parser [NixAST]
string = simpleString <|> indentedString

brackets :: Parser [NixAST] -> Parser [NixAST]
brackets p = symbol TBrackOpen <> p <> symbol TBrackClose

braces :: Parser [NixAST] -> Parser [NixAST]
braces p = symbol TBraceOpen <> p <> symbol TBraceClose

parens :: Parser [NixAST] -> Parser [NixAST]
parens p = symbol TParenOpen <> p <> symbol TParenClose

attrParameter :: Parser [NixAST]
attrParameter = node AttrParameter $
    identifier <> optional (symbol TQuestion <> expression)

setParameter :: Parser [NixAST]
setParameter = try $ node SetParameter $ braces $ optional $
    manyList (try $ attrParameter <> symbol TComma) <>
    (attrParameter <|> symbol TEllipsis)

contextParameter :: Parser [NixAST]
contextParameter = node ContextParameter $
    try (setParameter <> symbol TAt <> identifier) <|>
    try (identifier <> symbol TAt <> setParameter)

abstraction :: Parser [NixAST]
abstraction = node Abstraction $ try (
    (contextParameter <|> setParameter <|> identifier) <>
    symbol TColon) <> expression

nixInherit :: Parser [NixAST]
nixInherit = node Inherit $
    reserved TInherit <> optional nixParens <> manyList identifier <>
    symbol TSemicolon

assignment :: Parser [NixAST]
assignment = node Assignment $
    selectorPath <> symbol TAssign <> expression <> symbol TSemicolon

binders :: Parser [NixAST]
binders = manyList (assignment <|> nixInherit)

nixWith :: Parser [NixAST]
nixWith = node With $
    reserved TWith <> expression <> symbol TSemicolon <> expression

nixLet :: Parser [NixAST]
nixLet = try $ node Let $
    reserved TLet <> binders <> reserved TIn <> expression

ifThenElse :: Parser [NixAST]
ifThenElse = node If $
    reserved TIf <> expression <>
    reserved TThen <> expression <>
    reserved TElse <> expression

nixSet :: Parser [NixAST]
nixSet = try $ node Set $
    optional (reserved TRec) <> (braces binders)

nixValue :: Parser [NixAST]
nixValue = lexeme (nixSearchPath <|> nixPath <|> nixInt)

term :: Parser [NixAST]
term = selectorPath <|> nixValue <|> string <|>
    nixParens <|> nixSet <|> nixList <?> "term"

nixList :: Parser [NixAST]
nixList = try $ node List $ brackets $ manyList term

nixParens :: Parser [NixAST]
nixParens = try $ node Parenthesized $ parens $ expression

selector :: Parser [NixAST]
selector = try $ node Selector $ symbol TDot <>
    (identifier <|> interpolation <|> simpleString) <>
    optional (symbol TOr <> term)

selectorPath :: Parser [NixAST]
selectorPath = try $ node SelectorPath $ identifier <> manyList selector

opChars :: String
opChars = "<>=+-*/"

operator :: NixToken -> Parser [NixAST]
operator t = try (symbol t <* notFollowedBy (oneOf opChars)) <?> "operator"

opCombiner :: Operator -> MPExpr.Operator Parser [NixAST]
opCombiner Apply = MPExpr.InfixL $ return $ \x y -> [Node Application (x <> y)]
opCombiner (Op Prefix tok) = MPExpr.Prefix $ do
    parsed <- operator tok
    return (<> parsed)

opCombiner (Op Postfix TQuestion) = MPExpr.Postfix $ do
    tok <- operator TQuestion
    sel <- selector
    return (<> (tok <> sel))

opCombiner (Op Postfix _) = undefined

opCombiner (Op InfixL tok) = MPExpr.InfixL $ do
    parsed <- operator tok
    return $ \x y -> x <> parsed <> y

opCombiner (Op InfixN tok) = MPExpr.InfixN $ do
    parsed <- operator tok
    return $ \x y -> x <> parsed <> y

opCombiner (Op InfixR tok) = MPExpr.InfixR $ do
    parsed <- operator tok
    return $ \x y -> x <> parsed <> y

operation :: Parser [NixAST]
operation = MPExpr.makeExprParser term $ map (map opCombiner) operators

expression :: Parser [NixAST]
expression = abstraction <|> nixWith <|> nixLet <|> ifThenElse <|>
    operation <?> "expression"

nixFile :: Parser NixAST
nixFile = file expression
