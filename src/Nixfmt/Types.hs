{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Nixfmt.Types (
  ParseErrorBundle,
  Trivia,
  Ann (.., LoneAnn),
  ann,
  BinderF (..),
  Binder,
  ExpressionF (..),
  Expression,
  File,
  Fixity (..),
  Item (..),
  Items (..),
  Leaf,
  Operator (..),
  ParamAttrF (..),
  ParamAttr,
  ParameterF (..),
  Parameter,
  Parser,
  PathF,
  Path,
  SelectorF (..),
  Selector,
  SimpleSelectorF (..),
  SimpleSelector,
  StringPartF (..),
  StringPart,
  TermF (..),
  Term,
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
  operators,
  tokenText,
  walkSubprograms,
) where

import Control.Monad.State (StateT)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Pos)
import qualified Text.Megaparsec as MP (ParseErrorBundle, Parsec, pos1)
import Prelude hiding (String)
import Data.Monoid (First(..), Last (..))
import Data.Functor.Compose (Compose(..))

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
  deriving (Show, Functor, Foldable, Traversable)

removeLineInfo :: Ann a -> Ann a
removeLineInfo (Ann{preTrivia, value, trailComment}) =
  Ann
    { preTrivia,
      sourceLine = MP.pos1,
      value,
      trailComment
    }

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
  Ann{value = x} == Ann{value = y} = x == y

-- Trivia is ignored for Eq, so also don't show
-- instance Show a => Show (Ann a) where
--    show (Ann _ a _) = show a

data Item a
  = -- | An item
    Item a
  | -- | Trivia interleaved in items
    Comments Trivia
  deriving (Foldable, Show, Functor, Traversable)

newtype Items a = Items {unItems :: [Item a]} deriving (Functor, Foldable, Traversable)

instance (Eq a) => Eq (Items a) where
  (==) = (==) `on` concatMap Data.Foldable.toList . unItems

instance (Show a) => Show (Items a) where
  -- show = show . concatMap Data.Foldable.toList . unItems
  show = show . unItems

type Leaf = Ann Token

-- Map the first token of that expression, no matter how deep it sits
-- in the AST. This is useful for modifying comments
mapFirstToken :: Traversable e => (a -> a) -> e a -> e a
mapFirstToken f e = ifFirst $ traverse (\a -> IfFirstElse (f a) a) e

-- Same as mapFirstToken, but the mapping function also yields a value that may be
-- returned. This is useful for getting/extracting values
mapFirstToken' :: Traversable e => (a -> (a, c)) -> e a -> (e a, c)
mapFirstToken' f e = (ifFirst z, fromMaybe (error "Not a single element (first)") y) where
  Compose (First y, z) = traverse g e
  g a = Compose (First (Just c), IfFirstElse a' a) where
    (a', c) = f a

-- Same as mapLastToken, but the mapping function also yields a value that may be
-- returned. This is useful for getting/extracting values
mapLastToken' :: (Show (e a), Traversable e) => (a -> (a, c)) -> e a -> (e a, c)
mapLastToken' f e = (ifLast z, fromMaybe (error $ "Not a single element in " ++ show e) y) where
  Compose (Last y, z) = traverse g e
  g a = Compose (Last (Just c), IfLastElse a' a) where
    (a', c) = f a

-- | An Applicative which can make a value depend on whether it's the first one
data IfFirstElse a = IfFirstElsePure a | IfFirstElse a a deriving (Show, Functor)

ifFirst :: IfFirstElse a -> a
ifFirst (IfFirstElsePure a) = a
ifFirst (IfFirstElse a _) = a

instance Applicative IfFirstElse where
  pure = IfFirstElsePure

  IfFirstElse f1 f2 <*> IfFirstElse _ a2 = IfFirstElse (f1 a2) (f2 a2)
  f <*> IfFirstElsePure a = ($ a) <$> f
  IfFirstElsePure f <*> a = f <$> a

-- | An Applicative which can make a value depend on whether it's the last one
data IfLastElse a = IfLastElsePure a | IfLastElse a a deriving (Show, Functor)

ifLast :: IfLastElse a -> a
ifLast (IfLastElsePure a) = a
ifLast (IfLastElse a _) = a

instance Applicative IfLastElse where
  pure = IfLastElsePure

  IfLastElse _ f <*> IfLastElse a1 a2 = IfLastElse (f a1) (f a2)
  f <*> IfLastElsePure a = ($ a) <$> f
  IfLastElsePure f <*> a = f <$> a




data StringPartF a
  = TextPart a
  | Interpolation (Whole ExpressionF a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
type StringPart = StringPartF Leaf

type PathF a = [StringPartF a] --   ./foo/bar/${/* */baz}/florp # florp
type Path = PathF Leaf

-- A string consists of lines, each of which consists of text elements and interpolations.
-- The string's text does describe the raw input text value, and not the actual text it represents
-- within Nix semantics.
type StringF a = [StringPartF a]

data SimpleSelectorF a
  = IDSelector a
  | InterpolSelector (StringPartF a)
  | StringSelector (StringF a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
type SimpleSelector = SimpleSelectorF Leaf


data SelectorF a
  = -- `.selector`
    Selector (Maybe a) (SimpleSelectorF a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Selector = SelectorF Leaf

data BinderF a
  = Inherit a (Maybe (TermF a)) [SimpleSelectorF a] a
  | Assignment [SelectorF a] a (ExpressionF a) a
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Binder = BinderF Leaf

data TermF a
  = Token a
  | -- " String
    SimpleString (StringF a)
  | -- '' String
    IndentedString (StringF a)
  | Path (PathF a)
  | List a (Items (TermF a)) a
  | Set (Maybe a) a (Items (BinderF a)) a
  | Selection (TermF a) [SelectorF a] (Maybe (a, TermF a))
  | Parenthesized a (ExpressionF a) a
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Term = TermF Leaf

data ParamAttrF a
  = -- name, Maybe question mark and default, maybe comma
    ParamAttr a (Maybe (a, ExpressionF a)) (Maybe a)
  | ParamEllipsis a
  deriving (Eq, Show, Functor, Foldable, Traversable)
type ParamAttr = ParamAttrF Leaf

data ParameterF a
  = IDParameter a
  | SetParameter a [ParamAttrF a] a
  | ContextParameter (ParameterF a) a (ParameterF a)
  deriving (Show, Functor, Foldable, Traversable)
type Parameter = ParameterF Leaf

instance Eq a => Eq (ParameterF a) where
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

data ExpressionF a
  = Term (TermF a)
  | With a (ExpressionF a) a (ExpressionF a)
  | Let a (Items (BinderF a)) a (ExpressionF a)
  | Assert a (ExpressionF a) a (ExpressionF a)
  | If a (ExpressionF a) a (ExpressionF a) a (ExpressionF a)
  | Abstraction (ParameterF a) a (ExpressionF a)
  | Application (ExpressionF a) (ExpressionF a)
  | Operation (ExpressionF a) a (ExpressionF a)
  | MemberCheck (ExpressionF a) a [SelectorF a]
  | Negation a (ExpressionF a)
  | Inversion a (ExpressionF a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Expression = ExpressionF Leaf

-- | A Whole a is an a including final trivia. It's assumed the a stores the
-- initial trivia.
data Whole e a
  = Whole (e a) Trivia
  deriving (Show, Functor, Traversable, Foldable)

-- | Equality of annotated syntax is defined as equality of their corresponding
-- semantics, thus ignoring the annotations.
instance (Eq (e a)) => Eq (Whole e a) where
  Whole x _ == Whole y _ = x == y

-- Trivia is ignored for Eq, so also don't show
--instance (Show (e a)) => Show (Whole e a) where
--  show (Whole a _) = show a

type File = Whole ExpressionF Leaf

-- Implemented by all AST-related types whose values are guaranteed to contain at least one (annotated) token
class LanguageElement e where

  -- Walk all syntactically valid sub-expressions in a breadth-first search way. This allows
  -- minimizing failing test cases
  walkSubprograms :: e Leaf -> [ExpressionF Leaf]

--instance LanguageElement Ann where
--  mapFirstToken' f = f
--  mapLastToken' f = f
--  walkSubprograms = error "unreachable"
--  mapAllTokens f = f

instance LanguageElement SimpleSelectorF where
  walkSubprograms = \case
    (IDSelector name) -> [Term (Token name)]
    --(InterpolSelector Ann{sourceLine, value = str}) -> pure $ Term $ SimpleString $ ann sourceLine [[str]]
    (StringSelector str) -> [Term (SimpleString str)]

instance LanguageElement SelectorF where
  walkSubprograms (Selector _ ident) = walkSubprograms ident

instance LanguageElement BinderF where
  walkSubprograms _ = error "unused"

instance LanguageElement ParamAttrF where
  walkSubprograms = \case
    (ParamAttr name Nothing _) -> [Term (Token name)]
    (ParamAttr name (Just (_, def)) _) -> [Term (Token name), def]
    (ParamEllipsis _) -> []

instance LanguageElement ParameterF where
  walkSubprograms = \case
    (IDParameter ident) -> [Term $ Token ident]
    (SetParameter _ bindings _) -> bindings >>= walkSubprograms
    (ContextParameter left _ right) -> walkSubprograms left ++ walkSubprograms right

instance LanguageElement TermF where
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

instance LanguageElement ExpressionF where
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

instance LanguageElement e => LanguageElement (Whole e) where
  walkSubprograms (Whole a _) = walkSubprograms a

instance LanguageElement NonEmpty where
  walkSubprograms = error "unreachable"

data Token
  = Integer Int
  | Float Double
  | String Text
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
tokenText (String t) = t
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
