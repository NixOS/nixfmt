{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Parser where

import Control.Monad (guard, liftM2, (<$!>))
import Control.Monad.Combinators (sepBy)
import qualified Control.Monad.Combinators.Expr as MPExpr (
  Operator (..),
  makeExprParser,
 )
import Control.Monad.State.Strict (gets)
import Data.Bifunctor (second)
import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text, elem, isPrefixOf, pack)
import qualified Data.Text as Text
import Data.Void (Void)
import Nixfmt.Lexer (lexeme, takeTrivia, whole, wholeInner)
import Nixfmt.Parser.Float (floatParse)
import Nixfmt.Types (
  Ann (..),
  Binder (..),
  Expression (..),
  File,
  Fixity (..),
  Item (..),
  Items (..),
  Leaf,
  Operator (..),
  ParamAttr (..),
  Parameter (..),
  Parser,
  ParserState (..),
  Path,
  Selector (..),
  SimpleSelector (..),
  StringPart (..),
  Term (..),
  Token (..),
  Whole (..),
  operators,
  tokenText,
 )
import Nixfmt.Util (
  commonIndentation,
  identChar,
  isSpaces,
  manyP,
  manyText,
  pathChar,
  schemeChar,
  someP,
  someText,
  uriChar,
 )
import Text.Megaparsec (
  Parsec,
  anySingle,
  chunk,
  empty,
  eof,
  label,
  lookAhead,
  many,
  notFollowedBy,
  oneOf,
  optional,
  satisfy,
  some,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (char, digitChar)
import Prelude hiding (String, elem)

-- HELPER FUNCTIONS

ann :: (a -> b) -> Parser a -> Parser (Ann b)
ann f p = try $ lexeme $ f <$!> p

-- | parses a token without parsing trivia after it
rawSymbol :: Token -> Parser Token
rawSymbol t = chunk (tokenText t) $> t

symbol :: Token -> Parser (Ann Token)
symbol = lexeme . rawSymbol

reservedNames :: [Text]
reservedNames =
  [ "let",
    "in",
    "if",
    "then",
    "else",
    "assert",
    "with",
    "rec",
    "inherit"
  ]

reserved :: Token -> Parser (Ann Token)
reserved t =
  try $
    lexeme $
      rawSymbol t
        <* lookAhead (satisfy (\x -> not $ identChar x || pathChar x))

-- VALUES

integer :: Parser (Ann Token)
integer = ann Integer (pack <$> some digitChar)

float :: Parser (Ann Token)
float = ann Float floatParse

identifier :: Parser (Ann Token)
identifier = ann Identifier $ do
  ident <-
    Text.cons
      <$> satisfy (\x -> isAlpha x || x == '_')
      <*> manyP identChar
  guard $ ident `notElem` reservedNames
  return ident

slash :: Parser Text
slash = chunk "/" <* notFollowedBy (char '/')

instance (Semigroup a) => Semigroup (Parser a) where
  fx <> fy = do
    x <- fx
    y <- fy
    pure $! x <> y

envPath :: Parser (Ann Token)
envPath =
  ann EnvPath $
    char '<'
      *> someP pathChar <> manyText (slash <> someP pathChar)
      <* char '>'

pathText :: Parser StringPart
pathText = TextPart <$> someP pathChar

pathTraversal :: Parser [StringPart]
pathTraversal = liftM2 (:) (TextPart <$> slash) (some (pathText <|> interpolation))

path :: Parser Path
path =
  try $
    lexeme $
      fmap normalizeLine $
        (maybeToList <$> optional pathText) <> (concat <$> some pathTraversal)

uri :: Parser [[StringPart]]
uri =
  fmap (pure . pure . TextPart) $
    try $
      someP schemeChar <> chunk ":" <> someP uriChar

-- STRINGS

interpolation :: Parser StringPart
interpolation =
  Interpolation
    <$> (rawSymbol TInterOpen *> wholeInner expression <* rawSymbol TInterClose)

-- Interpolation, but only allowing identifiers and simple strings inside
interpolationRestricted :: Parser StringPart
interpolationRestricted = do
  interpol <- interpolation
  case interpol of
    -- Interpolation (Whole (Term (Token (Ann _ (Identifier _) _))) _) -> pure interpol
    Interpolation (Whole (Term (SimpleString _)) _) -> pure interpol
    _ -> empty

simpleStringPart :: Parser StringPart
simpleStringPart =
  TextPart
    <$> someText
      ( chunk "\\n"
          <|> chunk "\\r"
          <|> chunk "\\t"
          <|> ((<>) <$> chunk "\\" <*> (Text.singleton <$> anySingle))
          <|> chunk "$$"
          <|> try (chunk "$" <* notFollowedBy (char '{'))
          <|> someP (\t -> t /= '"' && t /= '\\' && t /= '$')
      )

indentedStringPart :: Parser StringPart
indentedStringPart =
  TextPart
    <$> someText
      ( {-
          This should match https://github.com/NixOS/nix/blob/052f1320ddf72d617e337479ff1bf22cb4ee682a/src/libexpr/lexer.l#L182-L205
          To understand that file, [this](https://westes.github.io/flex/manual/Matching.html#Matching) is important:
          > If it finds more than one match, it takes the one matching the most text.
          > If it finds two or more matches of the same length, the rule listed first in the flex input file is chosen.

          There are some inconsequential differences though:
          - We only parse one line at a time here, so we make sure to not process any newlines
          - We don't want to transform the input in any way, e.g. don't turn ''' into ''
            This is to preserve the input string as-is
        -}

        -- <IND_STRING>([^\$\']|\$[^\{\']|\'[^\'\$])+
        -- While this rule doesn't have a fixed length, it's non-conflicting with the rules below,
        -- so we can do it first without worrying about the length matching
        someText
          ( Text.singleton
              <$> satisfy (\t -> t /= '$' && t /= '\'' && t /= '\n')
                <|> try (Text.snoc <$> chunk "$" <*> satisfy (\t -> t /= '{' && t /= '\'' && t /= '\n'))
                <|> try (Text.snoc <$> chunk "'" <*> satisfy (\t -> t /= '\'' && t /= '$' && t /= '\n'))
          )
          -- These rules are of length 3, they need to come before shorter ones
          -- <IND_STRING>\'\'\$
          <|> chunk "''$"
          -- <IND_STRING>\'\'\'
          <|> chunk "'''"
          -- <IND_STRING>\'\'\\{ANY} -> Note that ANY can match newlines as well, but we need to ignore those
          <|> do
            prefix <- chunk "''\\"
            -- If we do have a newline
            rest <-
              -- If there's no newline, take the next character
              (notFollowedBy (char '\n') *> (Text.singleton <$> anySingle))
                -- Otherwise there's an unconsumed newline, which we don't need to handle,
                -- it's consumed elsewhere
                <|> pure ""
            pure $ prefix <> rest
          -- These are rules with length 2 and 1
          -- <IND_STRING>\$\{ -> don't match, this will be an interpolation
          -- <IND_STRING>\$ -> do match, just a dollar
          <|> try (chunk "$" <* notFollowedBy (char '{'))
          -- <IND_STRING>\'\' -> don't match, indented string ends
          -- <IND_STRING>\' -> do match, just a quote
          <|> try (chunk "'" <* notFollowedBy (char '\''))
      )

indentedLine :: Parser [StringPart]
indentedLine = many (indentedStringPart <|> interpolation)

isEmptyLine :: [StringPart] -> Bool
isEmptyLine [] = True
isEmptyLine [TextPart t] = isSpaces t
isEmptyLine _ = False

-- | Drop the first line of a string if it is empty.
-- However, don't drop it if it is the only line (empty string)
fixFirstLine :: [[StringPart]] -> [[StringPart]]
fixFirstLine [] = []
fixFirstLine (x : xs) = if isEmptyLine x' && not (null xs) then xs else x' : xs
  where
    x' = normalizeLine x

-- | Empty the last line if it contains only spaces.
fixLastLine :: [[StringPart]] -> [[StringPart]]
fixLastLine [] = []
fixLastLine [line] = if isEmptyLine line' then [[]] else [line']
  where
    line' = normalizeLine line
fixLastLine (x : xs) = x : fixLastLine xs

lineHead :: [StringPart] -> Maybe Text
lineHead [] = Nothing
lineHead line | isEmptyLine line = Nothing
lineHead (TextPart t : _) = Just t
lineHead (Interpolation{} : _) = Just ""

stripParts :: Text -> [StringPart] -> [StringPart]
stripParts indentation (TextPart t : xs) =
  TextPart (fromMaybe Text.empty $ Text.stripPrefix indentation t) : xs
stripParts _ xs = xs

-- | Split a list of StringParts on the newlines in their TextParts.
-- Invariant: result is never empty.
splitLines :: [StringPart] -> [[StringPart]]
splitLines [] = [[]]
splitLines (TextPart t : xs) =
  let ts = map (pure . TextPart) $ Text.split (== '\n') t
  in case splitLines xs of
       (xs' : xss) -> init ts ++ ((last ts ++ xs') : xss)
       _ -> error "unreachable"
splitLines (x : xs) =
  case splitLines xs of
    (xs' : xss) -> (x : xs') : xss
    _ -> error "unreachable"

-- | Strip the common indentation from all lines, returning it alongside.
stripIndentation :: [[StringPart]] -> (Text, [[StringPart]])
stripIndentation parts = case commonIndentation $ mapMaybe lineHead parts of
  Nothing -> ("", map (const []) parts)
  Just indentation -> (indentation, map (stripParts indentation) parts)

-- Merge adjacent string parts which resulted as parsing artifacts
normalizeLine :: [StringPart] -> [StringPart]
normalizeLine [] = []
normalizeLine (TextPart "" : xs) = normalizeLine xs
normalizeLine (TextPart x : TextPart y : xs) = normalizeLine (TextPart (x <> y) : xs)
normalizeLine (x : xs) = x : normalizeLine xs

fixSimpleString :: [StringPart] -> [[StringPart]]
fixSimpleString = map normalizeLine . splitLines

simpleString :: Parser [[StringPart]]
simpleString =
  rawSymbol TDoubleQuote
    *> fmap fixSimpleString (many (simpleStringPart <|> interpolation))
    <* rawSymbol TDoubleQuote

fixIndentedString :: [[StringPart]] -> (Text, [[StringPart]])
fixIndentedString =
  second (map normalizeLine . concatMap splitLines)
    . stripIndentation
    . fixLastLine
    . fixFirstLine

-- | The Maybe is the stripped indentation when a disabled region cuts through
-- the string (see 'Term'). The cut check sits between the '' delimiters so
-- that directives in the trivia after the string do not count.
indentedString :: Parser (Maybe Text, [[StringPart]])
indentedString = do
  before <- gets cutInterpolations
  (ind, parts) <-
    rawSymbol TDoubleSingleQuote
      *> fmap fixIndentedString (sepBy indentedLine (chunk "\n"))
      <* rawSymbol TDoubleSingleQuote
  cut <- gets ((/= before) . cutInterpolations)
  pure (if cut then Just ind else Nothing, parts)

-- | Parser for all string types (simple, URI, or indented)
string :: Parser Term
string =
  (SimpleString <$> lexeme (simpleString <|> uri))
    <|> (classifyString <$> lexeme indentedString)
  where
    -- Converts indented string syntax to appropriate string type.
    -- If the content can be represented as a simple string (no newlines, quotes or backslashes),
    -- it's reformatted as SimpleString to maintain a consistent style.
    -- Strings cut by a disabled region are exempt: the raw region text keeps
    -- their original '' syntax, so the delimiters must not change.
    classifyString s@Ann{value = (Nothing, parts)}
      | shouldBeSimpleString parts = SimpleString (s{value = convertIndentedEscapes parts})
    classifyString s@Ann{value = (cut, parts)} = IndentedString cut (s{value = parts})

    shouldBeSimpleString parts =
      not (containsNewlines parts) && not (any (any hasQuoteOrBackSlash) parts)

    containsNewlines parts = length parts > 1

    hasQuoteOrBackSlash (TextPart t) = '"' `elem` t || '\\' `elem` t
    hasQuoteOrBackSlash (Interpolation _) = False

    convertIndentedEscapes = map $ map convertPart

    convertPart (TextPart t) = TextPart (convertEscapes t)
    convertPart (Interpolation x) = Interpolation x

    -- Converts indented string escapes to simple string escapes
    convertEscapes t
      | Text.null t = t
      | "''$" `isPrefixOf` t = "\\$" <> convertEscapes (Text.drop 3 t) -- ''$ -> \$
      | "'''" `isPrefixOf` t = "''" <> convertEscapes (Text.drop 3 t) -- ''' -> ''
      | otherwise = Text.take 1 t <> convertEscapes (Text.drop 1 t)

-- TERMS

parens :: Parser Term
parens =
  Parenthesized
    <$> symbol TParenOpen
    <*> expression
    <*> symbol TParenClose

simpleSelector :: Parser StringPart -> Parser SimpleSelector
simpleSelector parseInterpolation =
  (IDSelector <$> identifier)
    <|> (InterpolSelector <$> lexeme parseInterpolation)
    <|> (StringSelector <$> lexeme simpleString)

selector :: Maybe (Parser Leaf) -> Parser Selector
selector parseDot =
  Selector
    <$> sequence parseDot
    <* notFollowedBy path
    <*> simpleSelector interpolation

selectorPath :: Parser [Selector]
selectorPath =
  (pure <$> selector Nothing)
    <> many (selector $ Just $ symbol TDot)

-- Path with a leading dot
selectorPath' :: Parser [Selector]
selectorPath' = many $ try $ selector $ Just $ symbol TDot

-- Everything but selection
simpleTerm :: Parser Term
simpleTerm =
  string
    <|> (Path <$> path)
    <|> (Token <$> (envPath <|> float <|> integer <|> identifier))
    <|> parens
    <|> set
    <|> list

term :: Parser Term
term = label "term" $ do
  !t <- simpleTerm
  sel <- selectorPath'
  case sel of
    [] -> return t
    _ -> do
      def <- optional (liftM2 (,) (reserved KOr) term)
      return $ Selection t sel def

items :: Parser a -> Parser (Items a)
items p = Items <$!> many (item p) <> (toList <$> optional itemComment)

item :: Parser a -> Parser (Item a)
item p = itemComment <|> Item <$!> p

itemComment :: Parser (Item a)
itemComment = do
  trivia <- takeTrivia
  case trivia of
    [] -> empty
    _ -> pure $ Comments trivia

-- ABSTRACTIONS

-- The lookahead ensures that a lone identifier is only accepted as parameter
-- when directly followed by the body (i.e. the colon, which is parsed by
-- `abstraction`); an `ident@` context instead backtracks into `setParameter`.
idParameter :: Parser Parameter
idParameter = IDParameter <$> identifier <* lookAhead (symbol TColon)

setParameter :: Parser Parameter
setParameter =
  -- Case 1: `ident@{}:`
  ( SetParameter
      <$> (Just <$> liftM2 (,) identifier (symbol TAt))
      <*> bopen
      <*> formals
      <*> bclose
      <*> pure Nothing
  )
    -- Case 2: `{}@ident:` and `{}:`
    <|> ( SetParameter Nothing
            <$> bopen
            <*> formals
            <*> bclose
            -- Either a context identifier follows, or the body (i.e. the
            -- colon, which is parsed by `abstraction`) must come directly.
            -- Once the `@` is consumed, the identifier is committed to.
            <*> ( (Just <$> liftM2 (,) (symbol TAt) identifier)
                    <|> (Nothing <$ lookAhead (symbol TColon))
                )
        )
  where
    bopen = symbol TBraceOpen
    bclose = symbol TBraceClose

    -- Also called a "formal" in the Nix codebase.
    attrParameter :: Parser ParamAttr
    attrParameter =
      ParamAttr
        <$> identifier
        <*> optional (liftM2 (,) (symbol TQuestion) expression)
        <*> optional (symbol TComma)

    -- Mirrors the `formals` rule of the Lix grammar.hh, but with the comma
    -- lookahead replaced by inspecting the comma of the formal just parsed.
    formals =
      (pure . ParamEllipsis <$> symbol TEllipsis)
        <|> ( attrParameter >>= \attr -> case attr of
                -- The comma means the list may continue (`{ a, b }`, `{ a, }`, `{ a, ... }`)
                -- Note: This parses a list via explicit recursion
                ParamAttr _ _ (Just _) -> (attr :) <$> formals
                -- No comma: this was the last formal (`{ a }`)
                _ -> pure [attr]
            )
        <|> pure []

abstraction :: Parser Expression
abstraction = Abstraction <$> (try idParameter <|> setParameter) <*> symbol TColon <*> expression

-- SETS AND LISTS

inherit :: Parser Binder
inherit =
  Inherit
    <$> reserved KInherit
    <*> optional parens
    <*> many (simpleSelector interpolationRestricted)
    <*> symbol TSemicolon

assignment :: Parser Binder
assignment =
  Assignment
    <$> selectorPath
    <*> symbol TAssign
    <*> expression
    <*> symbol TSemicolon

binders :: Parser (Items Binder)
binders = items (assignment <|> inherit)

set :: Parser Term
set =
  Set
    <$> optional (reserved KRec <|> reserved KLet)
    <*> symbol TBraceOpen
    <*> binders
    <*> symbol TBraceClose

list :: Parser Term
list = List <$> symbol TBrackOpen <*> items term <*> symbol TBrackClose

-- OPERATORS

operator :: Token -> Parser Leaf
operator t =
  label "operator" $
    try $
      lexeme $
        rawSymbol t
          <* notFollowedBy
            ( oneOf
                ( -- Resolve ambiguities between operators which are prefixes of others
                  case t of
                    TPlus -> "+" :: [Char]
                    TMinus -> ">"
                    TMul -> "/"
                    TDiv -> "/*"
                    TLess -> "=|"
                    TGreater -> "="
                    TNot -> "="
                    _ -> ""
                )
            )

-- | Parse a chain of prefix operators, used for numerical and boolean negation
chainPrefixOps :: Parser (Expression -> Expression) -> MPExpr.Operator Parser Expression
chainPrefixOps parser = MPExpr.Prefix do
  ops <- some parser
  pure \expr -> foldr ($) expr ops

opCombiner :: Operator -> MPExpr.Operator Parser Expression
opCombiner Apply = MPExpr.InfixL $ return Application
opCombiner (Op Prefix TMinus) = chainPrefixOps $ Negation <$> operator TMinus
opCombiner (Op Prefix TNot) = chainPrefixOps $ Inversion <$> operator TNot
opCombiner (Op Prefix _) = undefined
opCombiner (Op Postfix TQuestion) =
  MPExpr.Postfix $
    (\question sel expr -> MemberCheck expr question sel)
      <$> operator TQuestion
      <*> selectorPath
opCombiner (Op Postfix _) = undefined
-- HACK: We parse TPlus as left-associative, but convert it to right-associative in the AST
-- This is allowed because addition is fully associative
-- This is necessary because some downstream formatting code needs to match on the first operand,
-- and doing that with a left-associative chain is not possible.
-- A proper solution would be to flatten all associative operators into a list, but that would be a lot harder to do
opCombiner (Op InfixL TPlus) = MPExpr.InfixL $ flip mkOp <$> operator TPlus
  where
    mkOp :: Expression -> Leaf -> Expression -> Expression
    mkOp (Operation one op1 two) op2 three | op1 == op2 = Operation one op1 (Operation two op2 three)
    mkOp left op right = Operation left op right
opCombiner (Op InfixL tok) = MPExpr.InfixL $ flip Operation <$> operator tok
opCombiner (Op InfixN tok) = MPExpr.InfixN $ flip Operation <$> operator tok
opCombiner (Op InfixR tok) = MPExpr.InfixR $ flip Operation <$> operator tok

operation :: Parser Expression
operation =
  MPExpr.makeExprParser
    (Term <$> term <* notFollowedBy (oneOf (":@" :: [Char])))
    (map (map opCombiner) operators)

-- EXPRESSIONS

with :: Parser Expression
with =
  With
    <$> reserved KWith
    <*> expression
    <*> symbol TSemicolon
    <*> expression

letIn :: Parser Expression
letIn = Let <$> reserved KLet <*> binders <*> reserved KIn <*> expression

ifThenElse :: Parser Expression
ifThenElse =
  If
    <$> reserved KIf
    <*> expression
    <*> reserved KThen
    <*> expression
    <*> reserved KElse
    <*> expression

assert :: Parser Expression
assert =
  Assert
    <$> reserved KAssert
    <*> expression
    <*> symbol TSemicolon
    <*> expression

expression :: Parser Expression
expression =
  label "expression" $
    try operation
      <|> abstraction
      <|> with
      <|> letIn
      <|> ifThenElse
      <|> assert

file :: Parsec Void Text File
file = whole (expression <* eof)
