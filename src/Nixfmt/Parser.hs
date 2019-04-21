{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Parser where

import           Prelude                        hiding (String)

import           Control.Monad
import qualified Control.Monad.Combinators.Expr as MPExpr
import           Data.Char
import           Data.Foldable                  (toList)
import           Data.Text                      as Text hiding (concat, map)
import           Text.Megaparsec                hiding (Token)
import           Text.Megaparsec.Char           (char)
import           Text.Megaparsec.Char.Lexer     (decimal)

import           Nixfmt.Lexer
import           Nixfmt.Types
import           Nixfmt.Util

-- HELPER FUNCTIONS

ann :: (a -> b) -> Parser a -> Parser (Ann b)
ann f p = try $ lexeme $ f <$> p

pair :: Parser a -> Parser b -> Parser (a, b)
pair p q = (,) <$> p <*> q

-- | parses a token without parsing trivia after it
rawSymbol :: Token -> Parser Token
rawSymbol t = chunk (tokenText t) *> return t

symbol :: Token -> Parser (Ann Token)
symbol = lexeme . rawSymbol

reservedNames :: [Text]
reservedNames =
    [ "let", "in"
    , "if", "then", "else"
    , "assert"
    , "with"
    , "rec"
    , "inherit"
    ]

charClass :: [Char] -> Char -> Bool
charClass s c = isAlpha c || isDigit c || elem c s

identChar :: Char -> Bool
identChar = charClass "_'-"

pathChar :: Char -> Bool
pathChar = charClass "._-+~"

schemeChar :: Char -> Bool
schemeChar = charClass "-.+"

uriChar :: Char -> Bool
uriChar = charClass "~!@$%&*-=_+:',./?"

reserved :: Token -> Parser (Ann Token)
reserved t = try $ lexeme $ rawSymbol t
    <* lookAhead (satisfy (\x -> not $ identChar x || pathChar x))

-- VALUES

integer :: Parser (Ann Token)
integer = ann Integer decimal

identifier :: Parser (Ann Token)
identifier = ann Identifier $ do
    ident <- Text.cons <$> satisfy (\x -> isAlpha x || x == '_')
                       <*> manyP identChar
    guard $ not $ ident `elem` reservedNames
    return ident

slash :: Parser Text
slash = chunk "/" <* notFollowedBy (char '/')

envPath :: Parser (Ann Token)
envPath = ann EnvPath $ char '<' *>
    someP pathChar <> manyText (slash <> someP pathChar)
    <* char '>'

path :: Parser (Ann Token)
path = ann Path $ manyP pathChar <> someText (slash <> someP pathChar)

uri :: Parser String
uri = URIString <$> ann id (someP schemeChar <> chunk ":" <> someP uriChar)

-- STRINGS

interpolation :: Parser StringPart
interpolation = Interpolation <$>
    symbol TInterOpen <*> expression <*> rawSymbol TInterClose

simpleStringPart :: Parser StringPart
simpleStringPart = TextPart <$> someText (
    chunk "\\n" *> pure "\n" <|>
    chunk "\\r" *> pure "\r" <|>
    chunk "\\t" *> pure "\t" <|>
    chunk "\\" *> (Text.singleton <$> anySingle) <|>
    chunk "$$" <> manyP (=='$') <|>
    try (chunk "$" <* notFollowedBy (char '{')) <|>
    someP (\t -> t /= '"' && t /= '\\' && t /= '$'))

indentedStringPart :: Parser StringPart
indentedStringPart = TextPart <$> someText (
    chunk "''\\n" *> pure "\n" <|>
    chunk "''\\r" *> pure "\r" <|>
    chunk "''\\t" *> pure "\t" <|>
    chunk "''\\" *> (Text.singleton <$> anySingle) <|>
    chunk "''$" *> pure "$" <|>
    chunk "'''" *> pure "''" <|>
    chunk "$$" <> manyP (=='$') <|>
    try (chunk "$" <* notFollowedBy (char '{')) <|>
    try (chunk "'" <* notFollowedBy (char '\'')) <|>
    someP (\t -> t /= '\'' && t /= '$'))

simpleString :: Parser String
simpleString = SimpleString <$> rawSymbol TDoubleQuote <*>
    many (simpleStringPart <|> interpolation) <*>
    symbol TDoubleQuote

indentedString :: Parser String
indentedString = IndentedString <$> rawSymbol TDoubleSingleQuote <*>
    many (indentedStringPart <|> interpolation) <*>
    symbol TDoubleSingleQuote

string :: Parser String
string = simpleString <|> indentedString <|> uri

-- TERMS

parens :: Parser Term
parens = Parenthesized <$>
    symbol TParenOpen <*> expression <*> symbol TParenClose

selector :: Maybe (Parser Leaf) -> Parser Selector
selector parseDot = Selector <$> sequence parseDot <*>
    ((IDSelector <$> identifier) <|>
     (InterpolSelector <$> lexeme interpolation) <|>
     (StringSelector <$> simpleString)) <*>
    optional (pair (reserved KOr) term)

selectorPath :: Parser [Selector]
selectorPath = (pure <$> selector Nothing) <>
    many (selector $ Just $ symbol TDot)

simpleTerm :: Parser Term
simpleTerm = (String <$> string) <|>
    (Token <$> (path <|> envPath <|> integer <|> identifier)) <|>
    parens <|> set <|> list

term :: Parser Term
term = label "term" $ do
    t <- simpleTerm
    s <- many $ try $ selector $ Just $ symbol TDot
    return $ case s of [] -> t
                       _  -> Selection t s

-- ABSTRACTIONS

attrParameter :: Maybe (Parser Leaf) -> Parser ParamAttr
attrParameter parseComma = ParamAttr <$>
    identifier <*> optional (pair (symbol TQuestion) expression) <*>
    sequence parseComma

idParameter :: Parser Parameter
idParameter = IDParameter <$> identifier

setParameter :: Parser Parameter
setParameter = SetParameter <$> bopen <*> attrs <*> bclose
    where bopen      = symbol TBraceOpen
          bclose     = symbol TBraceClose
          commaAttrs = many $ try $ attrParameter $ Just $ symbol TComma
          ellipsis   = ParamEllipsis <$> symbol TEllipsis
          lastAttr   = attrParameter Nothing <|> ellipsis
          attrs      = commaAttrs <> (toList <$> optional (lastAttr))

contextParameter :: Parser Parameter
contextParameter =
    try (ContextParameter <$> setParameter <*> symbol TAt <*> idParameter) <|>
    try (ContextParameter <$> idParameter <*> symbol TAt <*> setParameter)

abstraction :: Parser Expression
abstraction = try (Abstraction <$>
    (contextParameter <|> setParameter <|> idParameter) <*>
    symbol TColon) <*> expression

-- SETS AND LISTS

inherit :: Parser Binder
inherit = Inherit <$> reserved KInherit <*> optional parens <*>
    many identifier <*> symbol TSemicolon

assignment :: Parser Binder
assignment = Assignment <$>
    selectorPath <*> symbol TAssign <*> expression <*> symbol TSemicolon

binders :: Parser [Binder]
binders = many (assignment <|> inherit)

set :: Parser Term
set = Set <$> optional (reserved KRec) <*>
    symbol TBraceOpen <*> binders <*> symbol TBraceClose

list :: Parser Term
list = List <$> symbol TBrackOpen <*>
    many (ListItem <$> term) <*> symbol TBrackClose

-- OPERATORS

opChars :: [Char]
opChars = "<>=+*/."

operator :: Token -> Parser Leaf
operator t = label "operator" $ try $ lexeme $
    rawSymbol t <* notFollowedBy (oneOf opChars)

opCombiner :: Operator -> MPExpr.Operator Parser Expression
opCombiner Apply = MPExpr.InfixL $ return Application

opCombiner (Op Prefix TMinus) = MPExpr.Prefix $ Negation <$> operator TMinus
opCombiner (Op Prefix TNot)   = MPExpr.Prefix $ Inversion <$> operator TNot
opCombiner (Op Prefix _)      = undefined

opCombiner (Op Postfix TQuestion) = MPExpr.Postfix $
    (\question sel expr -> MemberCheck expr question sel) <$>
    operator TQuestion <*> selectorPath

opCombiner (Op Postfix _) = undefined

opCombiner (Op InfixL tok) = MPExpr.InfixL $ flip Operation <$> operator tok
opCombiner (Op InfixN tok) = MPExpr.InfixN $ flip Operation <$> operator tok
opCombiner (Op InfixR tok) = MPExpr.InfixR $ flip Operation <$> operator tok

operation :: Parser Expression
operation = MPExpr.makeExprParser (Term <$> term) $
    map (map opCombiner) operators

-- EXPRESSIONS

with :: Parser Expression
with = With <$>
    reserved KWith <*> expression <*> symbol TSemicolon <*> expression

letIn :: Parser Expression
letIn = Let <$> reserved KLet <*> binders <*> reserved KIn <*> expression

ifThenElse :: Parser Expression
ifThenElse = If <$>
    reserved KIf <*> expression <*>
    reserved KThen <*> expression <*>
    reserved KElse <*> expression

assert :: Parser Expression
assert = Assert <$> reserved KAssert <*> expression <*>
    symbol TSemicolon <*> expression

expression :: Parser Expression
expression = (Term . String <$> uri) <|> abstraction <|>
    with <|> letIn <|> ifThenElse <|> assert <|>
    operation <?> "expression"

file :: Parser File
file = File <$> try (lexeme (return SOF)) <*> expression <* eof
