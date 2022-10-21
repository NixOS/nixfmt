{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Nixfmt.Parser where

import Prelude hiding (String)

import Control.Monad (guard, liftM2)
import Control.Monad.Combinators (sepBy)
import qualified Control.Monad.Combinators.Expr as MPExpr
  (Operator(..), makeExprParser)
import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text as Text (Text, cons, empty, singleton, split, stripPrefix)
import Text.Megaparsec
  (anySingle, chunk, eof, label, lookAhead, many, notFollowedBy, oneOf,
  optional, satisfy, some, try, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

import Nixfmt.Lexer (lexeme)
import Nixfmt.Types
  (Ann(..), Binder(..), Expression(..), File(..), Fixity(..), Leaf, Operator(..),
  ParamAttr(..), Parameter(..), Parser, Path, Selector(..), SimpleSelector(..),
  String, StringPart(..), Term(..), Token(..), operators, reservedNames,
  toLeading, tokenText)
import Nixfmt.Parser.Float (floatParse)
import Nixfmt.Util
  (commonIndentation, identChar, isSpaces, manyP, manyText, pathChar,
  schemeChar, someP, someText, uriChar)

-- HELPER FUNCTIONS

ann :: (a -> b) -> Parser a -> Parser (Ann b)
ann f p = try $ lexeme $ f <$> p

-- | parses a token without parsing trivia after it
rawSymbol :: Token -> Parser Token
rawSymbol t = chunk (tokenText t) *> return t

symbol :: Token -> Parser (Ann Token)
symbol = lexeme . rawSymbol

reserved :: Token -> Parser (Ann Token)
reserved t = try $ lexeme $ rawSymbol t
    <* lookAhead (satisfy (\x -> not $ identChar x || pathChar x))

-- VALUES

integer :: Parser (Ann Token)
integer = ann Integer L.decimal

float :: Parser (Ann Token)
float = ann Float floatParse

rawIdentifier :: Parser Text
rawIdentifier = Text.cons
    <$> satisfy (\x -> isAlpha x || x == '_')
    <*> manyP identChar

identifier :: Parser (Ann Token)
identifier = ann Identifier do
    ident <- rawIdentifier
    guard $ not $ ident `elem` reservedNames
    return ident

-- Support all of: { inherit foo "bar" ${"baz"}; }
fancyIdentifier :: Parser (Ann Token)
fancyIdentifier = identifier <|> stringIdentifier <|> curlyIdentifier

stringIdentifier :: Parser (Ann Token)
stringIdentifier = ann Identifier do
    rawSymbol TDoubleQuote *> rawIdentifier <* rawSymbol TDoubleQuote

curlyIdentifier :: Parser (Ann Token)
curlyIdentifier = do
    (Ann _ trailing1 leading1) <- symbol TInterOpen
    (Ann i trailing2 leading2) <- stringIdentifier
    (Ann _ trailing3 leading3) <- symbol TInterClose
    pure $ Ann i trailing1 $
      leading1 <> toLeading trailing2 <> leading2 <> toLeading trailing3 <> leading3

slash :: Parser Text
slash = chunk "/" <* notFollowedBy (char '/')

envPath :: Parser (Ann Token)
envPath = ann EnvPath $ char '<' *>
    someP pathChar <> manyText (slash <> someP pathChar)
    <* char '>'

pathText :: Parser StringPart
pathText = TextPart <$> someP pathChar

pathTraversal :: Parser [StringPart]
pathTraversal = liftM2 (:) (TextPart <$> slash) (some (pathText <|> interpolation))

path :: Parser Path
path = try $ lexeme $ fmap normalizeLine $
    (maybeToList <$> optional pathText) <> (concat <$> some pathTraversal)

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
    chunk "$$" <|>
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
    chunk "$$" <|>
    try (chunk "$" <* notFollowedBy (char '{')) <|>
    try (chunk "'" <* notFollowedBy (char '\'')) <|>
    someP (\t -> t /= '\'' && t /= '$' && t /= '\n'))

indentedLine :: Parser [StringPart]
indentedLine = many (indentedStringPart <|> interpolation)

isEmptyLine :: [StringPart] -> Bool
isEmptyLine []           = True
isEmptyLine [TextPart t] = isSpaces t
isEmptyLine _            = False

-- | Drop the first line of a string if it is empty.
fixFirstLine :: [[StringPart]] -> [[StringPart]]
fixFirstLine []       = []
fixFirstLine (x : xs) = if isEmptyLine x' then xs else x' : xs
    where x' = normalizeLine x

-- | Empty the last line if it contains only spaces.
fixLastLine :: [[StringPart]] -> [[StringPart]]
fixLastLine []       = []
fixLastLine [line]   = if isEmptyLine line' then [[]] else [line']
    where line' = normalizeLine line
fixLastLine (x : xs) = x : fixLastLine xs

lineHead :: [StringPart] -> Maybe Text
lineHead []                        = Nothing
lineHead line | isEmptyLine line   = Nothing
lineHead (TextPart t : _)          = Just t
lineHead (Interpolation _ _ _ : _) = Just ""

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
stripIndentation parts = case commonIndentation $ mapMaybe lineHead parts of
    Nothing -> map (const []) parts
    Just indentation -> map (stripParts indentation) parts

normalizeLine :: [StringPart] -> [StringPart]
normalizeLine [] = []
normalizeLine (TextPart "" : xs) = normalizeLine xs
normalizeLine (TextPart x : TextPart y : xs) = normalizeLine (TextPart (x <> y) : xs)
normalizeLine (x : xs) = x : normalizeLine xs

fixSimpleString :: [StringPart] -> [[StringPart]]
fixSimpleString = map normalizeLine . splitLines

simpleString :: Parser [[StringPart]]
simpleString = rawSymbol TDoubleQuote *>
    fmap fixSimpleString (many (simpleStringPart <|> interpolation)) <*
    rawSymbol TDoubleQuote

fixIndentedString :: [[StringPart]] -> [[StringPart]]
fixIndentedString
    = map normalizeLine
    . concatMap splitLines
    . stripIndentation
    . fixLastLine
    . fixFirstLine

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
selector parseDot = Selector <$>
    sequence parseDot <* notFollowedBy path <*>
    ((IDSelector <$> identifier) <|>
     (InterpolSelector <$> lexeme interpolation) <|>
     (StringSelector <$> lexeme simpleString)) <*>
    optional (liftM2 (,) (reserved KOr) term)

selectorPath :: Parser [Selector]
selectorPath = (pure <$> selector Nothing) <>
    many (selector $ Just $ symbol TDot)

simpleTerm :: Parser Term
simpleTerm = (String <$> string) <|> (Path <$> path) <|>
    (Token <$> (envPath <|> float <|> integer <|> identifier)) <|>
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
    many fancyIdentifier <*> symbol TSemicolon

assignment :: Parser Binder
assignment = Assignment <$>
    selectorPath <*> symbol TAssign <*> expression <*> symbol TSemicolon

binders :: Parser [Binder]
binders = many (assignment <|> inherit)

set :: Parser Term
set = Set <$> optional (reserved KRec <|> reserved KLet) <*>
    symbol TBraceOpen <*> binders <*> symbol TBraceClose

list :: Parser Term
list = List <$> symbol TBrackOpen <*> many term <*> symbol TBrackClose

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
