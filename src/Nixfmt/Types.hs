{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Nixfmt.Types (
  ParseErrorBundle,
  Trivia,
  Ann (.., LoneAnn),
  ann,
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
  Path,
  Selector (..),
  SimpleSelector (..),
  StringPart (..),
  Term (..),
  Token (..),
  Whole (..),
  TrailingComment (..),
  Trivium (..),
  removeLineInfo,
  hasTrivia,
  LanguageElement,
  mapFirstToken,
  mapFirstToken',
  mapLastToken',
  mapAllTokens,
  operators,
  tokenText,
  walkSubprograms,
) where

import Control.Monad.State.Strict (StateT)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Pos)
import qualified Text.Megaparsec as MP (ParseErrorBundle, Parsec, pos1)
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

data Ann a = Ann
  { preTrivia :: Trivia,
    -- | The line of this value in the source code
    sourceLine :: Pos,
    value :: a,
    trailComment :: Maybe TrailingComment
  }
  deriving (Show)

removeLineInfo :: Ann a -> Ann a
removeLineInfo a = a{sourceLine = MP.pos1}

-- | An annotated value without any trivia or trailing comment
pattern LoneAnn :: a -> Ann a
pattern LoneAnn a <- Ann [] _ a Nothing

hasTrivia :: Ann a -> Bool
hasTrivia (LoneAnn _) = False
hasTrivia _ = True

-- | Create a new annotated value without any annotations
ann :: Pos -> a -> Ann a
ann l v =
  Ann
    { preTrivia = [],
      sourceLine = l,
      value = v,
      trailComment = Nothing
    }

-- | Equality of annotated syntax is defined as equality of their corresponding
-- semantics, thus ignoring the annotations.
instance (Eq a) => Eq (Ann a) where
  (==) = (==) `on` value

data Item a
  = -- | An item
    Item a
  | -- | Trivia interleaved in items
    Comments Trivia
  deriving (Foldable, Show, Functor)

newtype Items a = Items {unItems :: [Item a]} deriving (Functor)

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

  -- Same as mapLastToken, but the mapping function also yields a value that may be
  -- returned. This is useful for getting/extracting values
  mapLastToken' :: (forall b. Ann b -> (Ann b, c)) -> a -> (a, c)

  mapAllTokens :: (forall b. Ann b -> Ann b) -> a -> a

  -- Walk all syntactically valid sub-expressions in a breadth-first search way. This allows
  -- minimizing failing test cases
  walkSubprograms :: a -> [Expression]

instance LanguageElement (Ann a) where
  mapFirstToken' f = f
  mapLastToken' f = f
  walkSubprograms = error "unreachable"
  mapAllTokens f = f

instance LanguageElement SimpleSelector where
  mapFirstToken' f = \case
    (IDSelector name) -> first IDSelector $ f name
    (InterpolSelector name) -> first InterpolSelector $ f name
    (StringSelector name) -> first StringSelector $ f name

  mapLastToken' = mapFirstToken'

  walkSubprograms = \case
    (IDSelector name) -> [Term (Token name)]
    (InterpolSelector Ann{sourceLine, value = str}) -> pure $ Term $ SimpleString $ ann sourceLine [[str]]
    (StringSelector str) -> [Term (SimpleString str)]

  mapAllTokens f = \case
    (IDSelector name) -> IDSelector $ f name
    (InterpolSelector name) -> InterpolSelector $ f name
    (StringSelector name) -> StringSelector $ f name

instance LanguageElement Selector where
  mapFirstToken' f (Selector Nothing ident) = first (Selector Nothing) $ mapFirstToken' f ident
  mapFirstToken' f (Selector (Just dot) ident) = first (\dot' -> Selector (Just dot') ident) $ mapFirstToken' f dot

  mapLastToken' f (Selector dot ident) = first (Selector dot) $ mapLastToken' f ident

  walkSubprograms (Selector _ ident) = walkSubprograms ident

  mapAllTokens f (Selector dot ident) = Selector (f <$> dot) (mapAllTokens f ident)

instance LanguageElement Binder where
  mapFirstToken' _ _ = error "unused"
  mapLastToken' _ _ = error "unused"
  walkSubprograms _ = error "unused"

  mapAllTokens f = \case
    (Inherit inherit from sels semicolon) -> Inherit (f inherit) (mapAllTokens f <$> from) (Prelude.map (mapAllTokens f) sels) (f semicolon)
    (Assignment sels eq rhs semicolon) -> Assignment (Prelude.map (mapAllTokens f) sels) (f eq) (mapAllTokens f rhs) (f semicolon)

instance LanguageElement ParamAttr where
  mapFirstToken' _ _ = error "unreachable"
  mapLastToken' _ _ = error "unreachable"

  walkSubprograms = \case
    (ParamAttr name Nothing _) -> [Term (Token name)]
    (ParamAttr name (Just (_, def)) _) -> [Term (Token name), def]
    (ParamEllipsis _) -> []

  mapAllTokens f = \case
    (ParamAttr name Nothing comma) -> ParamAttr (mapAllTokens f name) Nothing (f <$> comma)
    (ParamAttr name (Just (qmark, def)) comma) -> ParamAttr (mapAllTokens f name) (Just (f qmark, mapAllTokens f def)) (f <$> comma)
    (ParamEllipsis dots) -> ParamEllipsis $ f dots

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

  mapAllTokens f = \case
    (IDParameter name) -> IDParameter (f name)
    (SetParameter open items close) -> SetParameter (f open) (Prelude.map (mapAllTokens f) items) (f close)
    (ContextParameter first' at second) -> ContextParameter (mapAllTokens f first') (f at) (mapAllTokens f second)

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
    (List open items close) ->
      unItems items >>= \case
        Item item ->
          [Term (List (stripTrivia open) (Items [Item item]) (stripTrivia close))]
        Comments c ->
          [Term (List (stripTrivia open) (Items [Comments c]) (stripTrivia close))]
    (Set _ _ items _) | Prelude.length (unItems items) == 1 -> case Prelude.head (unItems items) of
      (Item (Inherit _ from sels _)) ->
        (Term <$> maybeToList from) ++ concatMap walkSubprograms sels
      (Item (Assignment sels _ expr _)) ->
        expr : concatMap walkSubprograms sels
      (Comments _) -> []
    (Set _ open items close) ->
      unItems items >>= \case
        -- Map each binding to a singleton set
        (Item item) ->
          [Term (Set Nothing (stripTrivia open) (Items [Item item]) (stripTrivia close))]
        (Comments c) ->
          [Term (Set Nothing (stripTrivia open) (Items [Comments c]) (stripTrivia close))]
    (Selection term sels Nothing) -> Term term : (sels >>= walkSubprograms)
    (Selection term sels (Just (_, def))) -> Term term : (sels >>= walkSubprograms) ++ [Term def]
    (Parenthesized _ expr _) -> [expr]
    -- The others are already minimal
    _ -> []
    where
      -- TODO: Don't do this stripping at all, Doesn't seem very critical
      stripTrivia a = a{preTrivia = [], trailComment = Nothing}

  mapAllTokens f = \case
    (Token leaf) -> Token (f leaf)
    (SimpleString string) -> SimpleString (f string)
    (IndentedString string) -> IndentedString (f string)
    (Path path) -> Path (f path)
    (List open items close) -> List (f open) (mapAllTokens f <$> items) (f close)
    (Set rec open items close) -> Set (f <$> rec) (f open) (mapAllTokens f <$> items) (f close)
    (Selection term sels (Just (orToken, def))) -> Selection (mapAllTokens f term) (Prelude.map (mapAllTokens f) sels) $ Just (f orToken, mapAllTokens f def)
    (Selection term sels Nothing) -> Selection (mapAllTokens f term) (Prelude.map (mapAllTokens f) sels) Nothing
    (Parenthesized open expr close) -> Parenthesized (f open) (mapAllTokens f expr) (f close)

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
    (Let Ann{sourceLine = startLine} items Ann{sourceLine = endLine} body) ->
      body
        : ( unItems items >>= \case
              -- Map each binding to a singleton set
              (Item item) -> [Term (Set Nothing (ann startLine TBraceOpen) (Items [Item item]) (ann endLine TBraceClose))]
              (Comments _) -> []
          )
    (Assert _ cond _ body) -> [cond, body]
    (If _ expr0 _ expr1 _ expr2) -> [expr0, expr1, expr2]
    -- If the body is just a token, it doesn't have an expression, only walk through the parameter
    -- This is also the base case for the result below
    (Abstraction param _ (Term (Token _))) -> walkSubprograms param
    -- Otherwise, to separate the parameter from the body while keeping it a valid expression,
    -- replace the body with just a token. Return the body (a valid expression on its own) separately
    (Abstraction param colon@Ann{sourceLine} body) -> [Abstraction param colon (Term (Token (ann sourceLine (Identifier "_")))), body]
    (Application g a) -> [g, a]
    (Operation left _ right) -> [left, right]
    (MemberCheck name _ sels) -> name : (sels >>= walkSubprograms)
    (Negation _ expr) -> [expr]
    (Inversion _ expr) -> [expr]

  mapAllTokens f = \case
    (Term term) -> Term (mapAllTokens f term)
    (With with expr0 semicolon expr1) -> With (f with) (mapAllTokens f expr0) (f semicolon) (mapAllTokens f expr1)
    (Let let_ items in_ body) -> Let (f let_) (mapAllTokens f <$> items) (f in_) (mapAllTokens f body)
    (Assert assert cond semicolon body) -> Assert (f assert) (mapAllTokens f cond) (f semicolon) (mapAllTokens f body)
    (If if_ expr0 then_ expr1 else_ expr2) -> If (f if_) (mapAllTokens f expr0) (f then_) (mapAllTokens f expr1) (f else_) (mapAllTokens f expr2)
    (Abstraction param colon body) -> Abstraction (mapAllTokens f param) (f colon) (mapAllTokens f body)
    (Application g a) -> Application (mapAllTokens f g) (mapAllTokens f a)
    (Operation left op right) -> Operation (mapAllTokens f left) (f op) (mapAllTokens f right)
    (MemberCheck name dot sels) -> MemberCheck (mapAllTokens f name) (f dot) (Prelude.map (mapAllTokens f) sels)
    (Negation not_ expr) -> Negation (f not_) (mapAllTokens f expr)
    (Inversion tilde expr) -> Inversion (f tilde) (mapAllTokens f expr)

instance LanguageElement (Whole Expression) where
  mapFirstToken' f (Whole a trivia) =
    first (`Whole` trivia) (mapFirstToken' f a)

  mapLastToken' f (Whole a trivia) =
    first (`Whole` trivia) (mapLastToken' f a)

  walkSubprograms (Whole a _) = [a]

  mapAllTokens f (Whole a trivia) = Whole (mapAllTokens f a) trivia

instance (LanguageElement a) => LanguageElement (NonEmpty a) where
  mapFirstToken' f (x :| _) = first pure $ mapFirstToken' f x

  mapLastToken' f (x :| []) = first pure $ mapLastToken' f x
  mapLastToken' f (x :| xs) = first ((x :|) . NonEmpty.toList) $ mapLastToken' f (NonEmpty.fromList xs)

  walkSubprograms = error "unreachable"

  mapAllTokens f = NonEmpty.map (mapAllTokens f)

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
  | TPipeForward
  | TPipeBackward
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
    [Op InfixL TPipeForward],
    [Op InfixR TPipeBackward]
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
tokenText TPipeForward = "|>"
tokenText TPipeBackward = "<|"
tokenText SOF = ""
