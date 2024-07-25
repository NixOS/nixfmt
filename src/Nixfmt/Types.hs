{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Nixfmt.Types where

import Control.Monad.State (StateT)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Data.Void (Void)
import qualified Text.Megaparsec as MP (ParseErrorBundle, Parsec)
import Prelude hiding (String)

-- | A @megaparsec@ @ParsecT@ specified for use with @nixfmt@.
type Parser = StateT Trivia (MP.Parsec Void Text)

-- | A @megaparsec@ @ParseErrorBundle@ specified for use with @nixfmt@.
type ParseErrorBundle = MP.ParseErrorBundle Text Void

data Trivium
  = EmptyLine
  | -- Single line comments, either with # or /*. (We don't need to track which one it is,
    -- as they will all be normalized to # comments.
    LineComment Text
  | -- Multi-line comments with /* or /**. Multiple # comments are treated as a list of `LineComment`.
    -- The bool indicates a doc comment (/**)
    BlockComment Bool [Text]
  deriving (Eq, Show)

type Trivia = [Trivium]

newtype TrailingComment = TrailingComment Text deriving (Eq, Show)

data Ann a
  = Ann Trivia a (Maybe TrailingComment)
  deriving (Show)

hasTrivia :: Ann a -> Bool
hasTrivia (Ann [] _ Nothing) = False
hasTrivia _ = True

ann :: a -> Ann a
ann a = Ann [] a Nothing

-- | Equality of annotated syntax is defined as equality of their corresponding
-- semantics, thus ignoring the annotations.
instance (Eq a) => Eq (Ann a) where
  Ann _ x _ == Ann _ y _ = x == y

-- Trivia is ignored for Eq, so also don't show
-- instance Show a => Show (Ann a) where
--    show (Ann _ a _) = show a

data Item a
  = -- | An item
    Item a
  | -- | Trivia interleaved in items
    Comments Trivia
  deriving (Foldable, Show)

newtype Items a = Items {unItems :: [Item a]}

instance (Eq a) => Eq (Items a) where
  (==) = (==) `on` concatMap Data.Foldable.toList . unItems

instance (Show a) => Show (Items a) where
  -- show = show . concatMap Data.Foldable.toList . unItems
  show = show . unItems

type Leaf = Ann Token

data StringPart
  = TextPart Text
  | Interpolation (Whole Expression)
  deriving (Eq, Show)

type Path = Ann [StringPart]

-- A string consists of lines, each of which consists of text elements and interpolations.
-- The string's text does describe the raw input text value, and not the actual text it represents
-- within Nix semantics.
type String = Ann [[StringPart]]

data SimpleSelector
  = IDSelector Leaf
  | InterpolSelector (Ann StringPart)
  | StringSelector String
  deriving (Eq, Show)

data Selector
  = -- `.selector`
    Selector (Maybe Leaf) SimpleSelector
  deriving (Eq, Show)

data Binder
  = Inherit Leaf (Maybe Term) [SimpleSelector] Leaf
  | Assignment [Selector] Leaf Expression Leaf
  deriving (Eq, Show)

data Term
  = Token Leaf
  | -- " String
    SimpleString String
  | -- '' String
    IndentedString String
  | Path Path
  | List Leaf (Items Term) Leaf
  | Set (Maybe Leaf) Leaf (Items Binder) Leaf
  | Selection Term [Selector] (Maybe (Leaf, Term))
  | Parenthesized Leaf Expression Leaf
  deriving (Eq, Show)

data ParamAttr
  = -- name, Maybe question mark and default, maybe comma
    ParamAttr Leaf (Maybe (Leaf, Expression)) (Maybe Leaf)
  | ParamEllipsis Leaf
  deriving (Eq, Show)

data Parameter
  = IDParameter Leaf
  | SetParameter Leaf [ParamAttr] Leaf
  | ContextParameter Parameter Leaf Parameter
  deriving (Show)

instance Eq Parameter where
  (IDParameter l) == (IDParameter r) = l == r
  (SetParameter l1 l2 l3) == (SetParameter r1 r2 r3) =
    l1 == r1
      && cmp l2 r2
      && l3 == r3
    where
      -- Compare two lists of paramters, but for the last argument don't compare whether or not there is a trailing comma
      cmp [] [] = True
      cmp [ParamAttr x1 x2 _] [ParamAttr y1 y2 _] = x1 == y1 && x2 == y2
      cmp (x : xs) (y : ys) = x == y && cmp xs ys
      cmp _ _ = False
  (ContextParameter l1 l2 l3) == (ContextParameter r1 r2 r3) = l1 == r1 && l2 == r2 && l3 == r3
  _ == _ = False

data Expression
  = Term Term
  | With Leaf Expression Leaf Expression
  | Let Leaf (Items Binder) Leaf Expression
  | Assert Leaf Expression Leaf Expression
  | If Leaf Expression Leaf Expression Leaf Expression
  | Abstraction Parameter Leaf Expression
  | Application Expression Expression
  | Operation Expression Leaf Expression
  | MemberCheck Expression Leaf [Selector]
  | Negation Leaf Expression
  | Inversion Leaf Expression
  deriving (Eq, Show)

-- | A Whole a is an a including final trivia. It's assumed the a stores the
-- initial trivia.
data Whole a
  = Whole a Trivia

-- | Equality of annotated syntax is defined as equality of their corresponding
-- semantics, thus ignoring the annotations.
instance (Eq a) => Eq (Whole a) where
  Whole x _ == Whole y _ = x == y

-- Trivia is ignored for Eq, so also don't show
instance (Show a) => Show (Whole a) where
  show (Whole a _) = show a

type File = Whole Expression

-- Implemented by all AST-related types whose values are guaranteed to contain at least one (annotated) token
class LanguageElement a where
  -- Map the first token of that expression, no matter how deep it sits
  -- in the AST. This is useful for modifying comments
  mapFirstToken :: (forall b. Ann b -> Ann b) -> a -> a
  mapFirstToken f a = fst (mapFirstToken' (\x -> (f x, ())) a)

  -- Same as mapFirstToken, but the mapping function also yields a value that may be
  -- returned. This is useful for getting/extracting values
  mapFirstToken' :: (forall b. Ann b -> (Ann b, c)) -> a -> (a, c)

  -- Map the last token of that expression, no matter how deep it sits
  -- in the AST. This is useful for modifying comments
  mapLastToken :: (forall b. Ann b -> Ann b) -> a -> a
  mapLastToken f a = fst (mapLastToken' (\x -> (f x, ())) a)

  -- Same as mapLastToken, but the mapping function also yields a value that may be
  -- returned. This is useful for getting/extracting values
  mapLastToken' :: (forall b. Ann b -> (Ann b, c)) -> a -> (a, c)

  -- Walk all syntactically valid sub-expressions in a breadth-first search way. This allows
  -- minimizing failing test cases
  walkSubprograms :: a -> [Expression]

instance LanguageElement (Ann a) where
  mapFirstToken' f = f
  mapLastToken' f = f
  walkSubprograms = error "unreachable"

instance LanguageElement SimpleSelector where
  mapFirstToken' f = \case
    (IDSelector name) -> first IDSelector $ f name
    (InterpolSelector name) -> first InterpolSelector $ f name
    (StringSelector name) -> first StringSelector $ f name

  mapLastToken' = mapFirstToken'

  walkSubprograms = \case
    (IDSelector name) -> [Term (Token name)]
    (InterpolSelector (Ann _ str _)) -> pure $ Term $ SimpleString $ Ann [] [[str]] Nothing
    (StringSelector str) -> [Term (SimpleString str)]

instance LanguageElement Selector where
  mapFirstToken' f (Selector Nothing ident) = first (Selector Nothing) $ mapFirstToken' f ident
  mapFirstToken' f (Selector (Just dot) ident) = first (\dot' -> Selector (Just dot') ident) $ mapFirstToken' f dot

  mapLastToken' f (Selector dot ident) = first (Selector dot) $ mapLastToken' f ident

  walkSubprograms (Selector _ ident) = walkSubprograms ident

instance LanguageElement ParamAttr where
  mapFirstToken' _ _ = error "unreachable"
  mapLastToken' _ _ = error "unreachable"

  walkSubprograms = \case
    (ParamAttr name Nothing _) -> [Term (Token name)]
    (ParamAttr name (Just (_, def)) _) -> [Term (Token name), def]
    (ParamEllipsis _) -> []

instance LanguageElement Parameter where
  mapFirstToken' f = \case
    (IDParameter name) -> first IDParameter (f name)
    (SetParameter open items close) -> first (\open' -> SetParameter open' items close) (f open)
    (ContextParameter first' at second) -> first (\first'' -> ContextParameter first'' at second) (mapFirstToken' f first')

  mapLastToken' f = \case
    (IDParameter name) -> first IDParameter (f name)
    (SetParameter open items close) -> first (SetParameter open items) (f close)
    (ContextParameter first' at second) -> first (ContextParameter first' at) (mapLastToken' f second)

  walkSubprograms = \case
    (IDParameter ident) -> [Term $ Token ident]
    (SetParameter _ bindings _) -> bindings >>= walkSubprograms
    (ContextParameter left _ right) -> walkSubprograms left ++ walkSubprograms right

instance LanguageElement Term where
  mapFirstToken' f = \case
    (Token leaf) -> first Token (f leaf)
    (SimpleString string) -> first SimpleString (f string)
    (IndentedString string) -> first IndentedString (f string)
    (Path path) -> first Path (f path)
    (List open items close) -> first (\open' -> List open' items close) (f open)
    (Set (Just rec) open items close) -> first (\rec' -> Set (Just rec') open items close) (f rec)
    (Set Nothing open items close) -> first (\open' -> Set Nothing open' items close) (f open)
    (Selection term selector def) -> first (\term' -> Selection term' selector def) (mapFirstToken' f term)
    (Parenthesized open expr close) -> first (\open' -> Parenthesized open' expr close) (f open)

  mapLastToken' f = \case
    (Token leaf) -> first Token (f leaf)
    (SimpleString string) -> first SimpleString (f string)
    (IndentedString string) -> first IndentedString (f string)
    (Path path) -> first Path (f path)
    (List open items close) -> first (List open items) (f close)
    (Set rec open items close) -> first (Set rec open items) (f close)
    (Selection term sels (Just (orToken, def))) -> first (\def' -> Selection term sels (Just (orToken, def'))) (mapLastToken' f def)
    (Selection term sels Nothing) ->
      case NonEmpty.nonEmpty sels of
        Just nonEmptySels -> first (\sels' -> Selection term (NonEmpty.toList sels') Nothing) (mapLastToken' f nonEmptySels)
        Nothing -> first (\term' -> Selection term' [] Nothing) (mapLastToken' f term)
    (Parenthesized open expr close) -> first (Parenthesized open expr) (f close)

  walkSubprograms = \case
    -- Map each item to a singleton list, then handle that
    (List _ items _) | Prelude.length (unItems items) == 1 -> case Prelude.head (unItems items) of
      (Item item) -> [Term item]
      (Comments _) -> []
    (List _ items _) ->
      unItems items >>= \case
        Item item ->
          [Term (List (ann TBrackOpen) (Items [Item item]) (ann TBrackClose))]
        Comments c ->
          [Term (List (ann TBrackOpen) (Items [Comments c]) (ann TBrackClose))]
    (Set _ _ items _) | Prelude.length (unItems items) == 1 -> case Prelude.head (unItems items) of
      (Item (Inherit _ from sels _)) ->
        (Term <$> maybeToList from) ++ concatMap walkSubprograms sels
      (Item (Assignment sels _ expr _)) ->
        expr : concatMap walkSubprograms sels
      (Comments _) -> []
    (Set _ _ items _) ->
      unItems items >>= \case
        -- Map each binding to a singleton set
        (Item item) ->
          [Term (Set Nothing (ann TBraceOpen) (Items [Item item]) (ann TBraceClose))]
        (Comments c) -> [emptySet c]
    (Selection term sels Nothing) -> Term term : (sels >>= walkSubprograms)
    (Selection term sels (Just (_, def))) -> Term term : (sels >>= walkSubprograms) ++ [Term def]
    (Parenthesized _ expr _) -> [expr]
    -- The others are already minimal
    _ -> []
    where
      emptySet c = Term (Set Nothing (ann TBraceOpen) (Items [Comments c]) (ann TBraceClose))

instance LanguageElement Expression where
  mapFirstToken' f = \case
    (Term term) -> first Term (mapFirstToken' f term)
    (With with expr0 semicolon expr1) -> first (\with' -> With with' expr0 semicolon expr1) (f with)
    (Let let_ items in_ body) -> first (\let_' -> Let let_' items in_ body) (f let_)
    (Assert assert cond semicolon body) -> first (\assert' -> Assert assert' cond semicolon body) (f assert)
    (If if_ expr0 then_ expr1 else_ expr2) -> first (\if_' -> If if_' expr0 then_ expr1 else_ expr2) (f if_)
    (Abstraction param colon body) -> first (\param' -> Abstraction param' colon body) (mapFirstToken' f param)
    (Application g a) -> first (`Application` a) (mapFirstToken' f g)
    (Operation left op right) -> first (\left' -> Operation left' op right) (mapFirstToken' f left)
    (MemberCheck name dot selectors) -> first (\name' -> MemberCheck name' dot selectors) (mapFirstToken' f name)
    (Negation not_ expr) -> first (`Negation` expr) (f not_)
    (Inversion tilde expr) -> first (`Inversion` expr) (f tilde)

  mapLastToken' f = \case
    (Term term) -> first Term (mapLastToken' f term)
    (With with expr0 semicolon expr1) -> first (With with expr0 semicolon) (mapLastToken' f expr1)
    (Let let_ items in_ body) -> first (Let let_ items in_) (mapLastToken' f body)
    (Assert assert cond semicolon body) -> first (Assert assert cond semicolon) (mapLastToken' f body)
    (If if_ expr0 then_ expr1 else_ expr2) -> first (If if_ expr0 then_ expr1 else_) (mapLastToken' f expr2)
    (Abstraction param colon body) -> first (Abstraction param colon) (mapLastToken' f body)
    (Application g a) -> first (Application g) (mapLastToken' f a)
    (Operation left op right) -> first (Operation left op) (mapLastToken' f right)
    (MemberCheck name dot []) -> first (\dot' -> MemberCheck name dot' []) (mapLastToken' f dot)
    (MemberCheck name dot sels) -> first (MemberCheck name dot . NonEmpty.toList) (mapLastToken' f $ NonEmpty.fromList sels)
    (Negation not_ expr) -> first (Negation not_) (mapLastToken' f expr)
    (Inversion tilde expr) -> first (Inversion tilde) (mapLastToken' f expr)

  walkSubprograms = \case
    (Term term) -> walkSubprograms term
    (With _ expr0 _ expr1) -> [expr0, expr1]
    (Let _ items _ body) ->
      body
        : ( unItems items >>= \case
              -- Map each binding to a singleton set
              (Item item) -> [Term (Set Nothing (ann TBraceOpen) (Items [Item item]) (ann TBraceClose))]
              (Comments _) -> []
          )
    (Assert _ cond _ body) -> [cond, body]
    (If _ expr0 _ expr1 _ expr2) -> [expr0, expr1, expr2]
    -- If the body is just a token, it doesn't have an expression, only walk through the parameter
    -- This is also the base case for the result below
    (Abstraction param _ (Term (Token _))) -> walkSubprograms param
    -- Otherwise, to separate the parameter from the body while keeping it a valid expression,
    -- replace the body with just a token. Return the body (a valid expression on its own) separately
    (Abstraction param colon body) -> [Abstraction param colon (Term (Token (ann (Identifier "_")))), body]
    (Application g a) -> [g, a]
    (Operation left _ right) -> [left, right]
    (MemberCheck name _ sels) -> name : (sels >>= walkSubprograms)
    (Negation _ expr) -> [expr]
    (Inversion _ expr) -> [expr]

instance LanguageElement (Whole Expression) where
  mapFirstToken' f (Whole a trivia) =
    first (`Whole` trivia) (mapFirstToken' f a)

  mapLastToken' f (Whole a trivia) =
    first (`Whole` trivia) (mapLastToken' f a)

  walkSubprograms (Whole a _) = [a]

instance (LanguageElement a) => LanguageElement (NonEmpty a) where
  mapFirstToken' f (x :| _) = first pure $ mapFirstToken' f x

  mapLastToken' f (x :| []) = first pure $ mapLastToken' f x
  mapLastToken' f (x :| xs) = first ((x :|) . NonEmpty.toList) $ mapLastToken' f (NonEmpty.fromList xs)

  walkSubprograms = error "unreachable"

data Token
  = Integer Int
  | Float Double
  | Identifier Text
  | EnvPath Text
  | KAssert
  | KElse
  | KIf
  | KIn
  | KInherit
  | KLet
  | KOr
  | KRec
  | KThen
  | KWith
  | TBraceOpen
  | TBraceClose
  | TBrackOpen
  | TBrackClose
  | TInterOpen
  | TInterClose
  | TParenOpen
  | TParenClose
  | TAssign
  | TAt
  | TColon
  | TComma
  | TDot
  | TDoubleQuote
  | TDoubleSingleQuote
  | TEllipsis
  | TQuestion
  | TSemicolon
  | TConcat
  | TNegate
  | TUpdate
  | TPlus
  | TMinus
  | TMul
  | TDiv
  | TAnd
  | TOr
  | TEqual
  | TGreater
  | TGreaterEqual
  | TImplies
  | TLess
  | TLessEqual
  | TNot
  | TUnequal
  | TPipeTo
  | TPipeFrom
  | SOF
  deriving (Eq, Show)

data Fixity
  = Prefix
  | InfixL
  | InfixN
  | InfixR
  | Postfix
  deriving (Eq, Show)

data Operator
  = Op Fixity Token
  | Apply
  deriving (Eq, Show)

-- | A list of lists of operators where lists that come first contain operators
-- that bind more strongly.
operators :: [[Operator]]
operators =
  [ [Apply],
    [Op Prefix TMinus],
    [Op Postfix TQuestion],
    [Op InfixR TConcat],
    [ Op InfixL TMul,
      Op InfixL TDiv
    ],
    [ Op InfixL TPlus,
      Op InfixL TMinus
    ],
    [Op Prefix TNot],
    [Op InfixR TUpdate],
    [ Op InfixN TLess,
      Op InfixN TGreater,
      Op InfixN TLessEqual,
      Op InfixN TGreaterEqual
    ],
    [ Op InfixN TEqual,
      Op InfixN TUnequal
    ],
    [Op InfixL TAnd],
    [Op InfixL TOr],
    [Op InfixL TImplies],
    [Op InfixL TPipeTo],
    [Op InfixR TPipeFrom]
  ]

tokenText :: Token -> Text
tokenText (Identifier i) = i
tokenText (Integer i) = pack (show i)
tokenText (Float f) = pack (show f)
tokenText (EnvPath p) = "<" <> p <> ">"
tokenText KAssert = "assert"
tokenText KElse = "else"
tokenText KIf = "if"
tokenText KIn = "in"
tokenText KInherit = "inherit"
tokenText KLet = "let"
tokenText KOr = "or"
tokenText KRec = "rec"
tokenText KThen = "then"
tokenText KWith = "with"
tokenText TBraceOpen = "{"
tokenText TBraceClose = "}"
tokenText TBrackOpen = "["
tokenText TBrackClose = "]"
tokenText TInterOpen = "${"
tokenText TInterClose = "}"
tokenText TParenOpen = "("
tokenText TParenClose = ")"
tokenText TAssign = "="
tokenText TAt = "@"
tokenText TColon = ":"
tokenText TComma = ","
tokenText TDot = "."
tokenText TDoubleQuote = "\""
tokenText TDoubleSingleQuote = "''"
tokenText TEllipsis = "..."
tokenText TQuestion = "?"
tokenText TSemicolon = ";"
tokenText TPlus = "+"
tokenText TMinus = "-"
tokenText TMul = "*"
tokenText TDiv = "/"
tokenText TConcat = "++"
tokenText TNegate = "-"
tokenText TUpdate = "//"
tokenText TAnd = "&&"
tokenText TOr = "||"
tokenText TEqual = "=="
tokenText TGreater = ">"
tokenText TGreaterEqual = ">="
tokenText TImplies = "->"
tokenText TLess = "<"
tokenText TLessEqual = "<="
tokenText TNot = "!"
tokenText TUnequal = "!="
tokenText TPipeTo = "|>"
tokenText TPipeFrom = "<|"
tokenText SOF = ""
