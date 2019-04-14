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

charClass :: String -> Char -> Bool
charClass s c = isAlpha c || isDigit c || elem c s

identChar :: Char -> Bool
identChar = charClass "_'-"

pathChar :: Char -> Bool
pathChar = charClass "._-+~"

schemeChar :: Char -> Bool
schemeChar = charClass "-.+"

uriChar :: Char -> Bool
uriChar = charClass "~!@$%&*-=_+:',./?"

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

slash :: Parser Text
slash = chunk "/" <* notFollowedBy (char '/')

nixSearchPath :: Parser NixToken
nixSearchPath = try $ EnvPath <$> (char '<' *>
    liftM2 Text.append
        (someP pathChar)
        (manyText $ slash <> someP pathChar)
    <* char '>')

nixPath :: Parser NixToken
nixPath = try $ NixURI <$>
    manyP pathChar <> someText (slash <> someP pathChar)

uri :: Parser [NixAST]
uri = try $ node URIString $ lexeme $ NixText <$>
    someP schemeChar <> chunk ":" <> someP uriChar

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
string = simpleString <|> indentedString <|> uri

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

assert :: Parser [NixAST]
assert = node Assert $ reserved TAssert <> expression <>
    symbol TSemicolon <> expression

nixSet :: Parser [NixAST]
nixSet = try $ node Set $
    optional (reserved TRec) <> (braces binders)

nixValue :: Parser [NixAST]
nixValue = lexeme (nixSearchPath <|> nixPath <|> nixInt)

simpleTerm :: Parser [NixAST]
simpleTerm = string <|> identifier <|> nixValue <|>
    nixParens <|> nixSet <|> nixList <?> "term"

nixList :: Parser [NixAST]
nixList = try $ node List $ brackets $ manyList term

nixParens :: Parser [NixAST]
nixParens = try $ node Parenthesized $ parens $ expression

selector :: Parser [NixAST]
selector = try $ node Selector $
    (identifier <|> interpolation <|> simpleString) <>
    optional (symbol TOr <> term)

selectorPath :: Parser [NixAST]
selectorPath = node SelectorPath $
    selector <> manyList (try $ symbol TDot <> selector)

term :: Parser [NixAST]
term = do
    t <- simpleTerm
    p <- optional $ try $ symbol TDot <> selectorPath
    return $ case p of [] -> t
                       _  -> [Node Selection (t <> p)]

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
expression = uri <|> abstraction <|>
    nixWith <|> nixLet <|> ifThenElse <|> assert <|>
    operation <?> "expression"

nixFile :: Parser NixAST
nixFile = file expression
