{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Nixfmt.Parser where

import Prelude hiding (String)

import Control.Monad (guard, liftM2)
import Control.Monad.Combinators (sepBy)
import qualified Control.Monad.Combinators.Expr as MPExpr
  (Operator(..), makeExprParser)
import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text as Text
  (Text, cons, empty, null, singleton, split, strip, stripPrefix)
import Text.Megaparsec
  (anySingle, chunk, eof, label, lookAhead, many, notFollowedBy, oneOf,
  optional, satisfy, try, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, float)

import Nixfmt.Lexer (lexeme)
import Nixfmt.Types
  (Ann, Binder(..), Expression(..), File(..), Fixity(..), Leaf, ListPart(..),
  Operator(..), ParamAttr(..), Parameter(..), Parser, Selector(..),
  SimpleSelector(..), String, StringPart(..), Term(..), Token(..), operators,
  tokenText)
import Nixfmt.Util
  (commonIndentation, identChar, manyP, manyText, pathChar, schemeChar, someP,
  someText, uriChar)

-- HELPER FUNCTIONS

ann :: (a -> b) -> Parser a -> Parser (Ann b)
ann f p = try $ lexeme $ f <$> p

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

reserved :: Token -> Parser (Ann Token)
reserved t = try $ lexeme $ rawSymbol t
    <* lookAhead (satisfy (\x -> not $ identChar x || pathChar x))

-- VALUES

integer :: Parser (Ann Token)
integer = ann Integer L.decimal

float :: Parser (Ann Token)
float = ann Float L.float

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

uri :: Parser [[StringPart]]
uri = fmap (pure . pure . TextPart) $ try $
    someP schemeChar <> chunk ":" <> someP uriChar

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
    someP (\t -> t /= '\'' && t /= '$' && t /= '\n'))

indentedLine :: Parser [StringPart]
indentedLine = many (indentedStringPart <|> interpolation)

isEmptyLine :: [StringPart] -> Bool
isEmptyLine []           = True
isEmptyLine [TextPart t] = Text.null (Text.strip t)
isEmptyLine _            = False

-- | Strip the first line of a string if it is empty.
stripFirstLine :: [[StringPart]] -> [[StringPart]]
stripFirstLine [] = []
stripFirstLine (x : xs)
    | isEmptyLine x = xs
    | otherwise     = x : xs

textHeads :: [StringPart] -> [Text]
textHeads line@(TextPart t : _)
    | isEmptyLine line              = []
    | otherwise                     = [t]
textHeads (Interpolation _ _ _ : _) = [""]
textHeads []                        = []

stripParts :: Text -> [StringPart] -> [StringPart]
stripParts indentation (TextPart t : xs) =
    TextPart (fromMaybe Text.empty $ Text.stripPrefix indentation t) : xs
stripParts _ xs = xs

-- | Split a list of StringParts on the newlines in their TextParts.
-- Invariant: result is never empty.
splitLines :: [StringPart] -> [[StringPart]]
splitLines [] = [[]]
splitLines (TextPart t : xs) =
    let ts = map (pure . TextPart) $ Text.split (=='\n') t
    in case splitLines xs of
        (xs' : xss) -> init ts ++ ((last ts ++ xs') : xss)
        _           -> error "unreachable"

splitLines (x : xs) =
    case splitLines xs of
        (xs' : xss) -> ((x : xs') : xss)
        _           -> error "unreachable"

stripIndentation :: [[StringPart]] -> [[StringPart]]
stripIndentation parts = case commonIndentation (concatMap textHeads parts) of
    Nothing -> map (const []) parts
    Just indentation -> map (stripParts indentation) parts

dropEmptyParts :: [[StringPart]] -> [[StringPart]]
dropEmptyParts = map $ filter (\case
    TextPart t | Text.null t -> False
    _                        -> True)

fixSimpleString :: [StringPart] -> [[StringPart]]
fixSimpleString parts = case splitLines parts of
    [] -> []
    [line] -> [line]
    parts' -> dropEmptyParts (stripIndentation parts')

simpleString :: Parser [[StringPart]]
simpleString = rawSymbol TDoubleQuote *>
    fmap splitLines (many (simpleStringPart <|> interpolation)) <*
    rawSymbol TDoubleQuote

fixIndentedString :: [[StringPart]] -> [[StringPart]]
fixIndentedString = dropEmptyParts . concatMap splitLines . stripIndentation . stripFirstLine

indentedString :: Parser [[StringPart]]
indentedString = rawSymbol TDoubleSingleQuote *>
    fmap fixIndentedString (sepBy indentedLine (chunk "\n")) <*
    rawSymbol TDoubleSingleQuote

string :: Parser String
string = lexeme $ simpleString <|> indentedString <|> uri

-- TERMS

parens :: Parser Term
parens = Parenthesized <$>
    symbol TParenOpen <*> expression <*> symbol TParenClose

selector :: Maybe (Parser Leaf) -> Parser Selector
selector parseDot = Selector <$> sequence parseDot <*>
    ((IDSelector <$> identifier) <|>
     (InterpolSelector <$> lexeme interpolation) <|>
     (StringSelector <$> lexeme simpleString)) <*>
    optional (liftM2 (,) (reserved KOr) term)

selectorPath :: Parser [Selector]
selectorPath = (pure <$> selector Nothing) <>
    many (selector $ Just $ symbol TDot)

simpleTerm :: Parser Term
simpleTerm = (String <$> string) <|>
    (Token <$> (path <|> envPath <|> float <|> integer <|> identifier)) <|>
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
    identifier <*> optional (liftM2 (,) (symbol TQuestion) expression) <*>
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
set = Set <$> optional (reserved KRec <|> reserved KLet) <*>
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
operation = MPExpr.makeExprParser
    (Term <$> term <* notFollowedBy (oneOf (":@" :: [Char])))
    (map (map opCombiner) operators)

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
expression = label "expression" $ try operation <|> abstraction <|>
    with <|> letIn <|> ifThenElse <|> assert

file :: Parser File
file = File <$> lexeme (return SOF) <*> expression <* eof
