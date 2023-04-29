{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveFoldable, OverloadedStrings #-}

module Nixfmt.Types where

import Prelude hiding (String)

import Control.Monad.State (StateT)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Text (Text, pack)
import Data.Void (Void)
import qualified Text.Megaparsec as MP (ParseErrorBundle, Parsec)

-- | A @megaparsec@ @ParsecT@ specified for use with @nixfmt@.
type Parser = StateT Trivia (MP.Parsec Void Text)

-- | A @megaparsec@ @ParseErrorBundle@ specified for use with @nixfmt@.
type ParseErrorBundle = MP.ParseErrorBundle Text Void

data Trivium
    = EmptyLine
    | LineComment     Text
    | BlockComment    [Text]
    deriving (Eq, Show)

type Trivia = [Trivium]

newtype TrailingComment = TrailingComment Text deriving (Eq, Show)

data Ann a
    = Ann Trivia a (Maybe TrailingComment)
    deriving (Show)

-- | Equality of annotated syntax is defined as equality of their corresponding
-- semantics, thus ignoring the annotations.
instance Eq a => Eq (Ann a) where
    Ann _ x _ == Ann _ y _ = x == y

data Item a
    -- | An item with a list of line comments that apply to it. There is no
    -- empty line between the comments and the stuff it applies to.
    = CommentedItem Trivia a
    -- | A list of line comments not associated with any item. Followed by an
    -- empty line unless they're the last comments in a set or list.
    | DetachedComments Trivia
    deriving (Foldable, Show)

newtype Items a = Items { unItems :: [Item a] }
    deriving (Show)

instance Eq a => Eq (Items a) where
    (==) = (==) `on` concatMap toList . unItems

type Leaf = Ann Token

data StringPart
    = TextPart Text
    | Interpolation (Whole Expression)
    deriving (Eq, Show)

type Path = Ann [StringPart]

type String = Ann [[StringPart]]

data SimpleSelector
    = IDSelector Leaf
    | InterpolSelector (Ann StringPart)
    | StringSelector String
    deriving (Eq, Show)

data Selector
    = Selector (Maybe Leaf) SimpleSelector (Maybe (Leaf, Term))
    deriving (Eq, Show)

data Binder
    = Inherit Leaf (Maybe Term) [Leaf] Leaf
    | Assignment [Selector] Leaf Expression Leaf
    deriving (Eq, Show)

data Term
    = Token Leaf
    | String String
    | Path Path
    | List Leaf (Items Term) Leaf
    | Set (Maybe Leaf) Leaf (Items Binder) Leaf
    | Selection Term [Selector]
    | Parenthesized Leaf Expression Leaf
    deriving (Eq, Show)

data ParamAttr
    = ParamAttr Leaf (Maybe (Leaf, Expression)) (Maybe Leaf)
    | ParamEllipsis Leaf
    deriving (Eq, Show)

data Parameter
    = IDParameter Leaf
    | SetParameter Leaf [ParamAttr] Leaf
    | ContextParameter Parameter Leaf Parameter
    deriving (Eq, Show)

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
    deriving (Eq, Show)

type File = Whole Expression

data Token
    = Integer    Int
    | Float      Double
    | Identifier Text
    | EnvPath    Text

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
    [ [ Apply ]
    , [ Op Prefix TMinus ]
    , [ Op Postfix TQuestion ]
    , [ Op InfixR TConcat ]
    , [ Op InfixL TMul
      , Op InfixL TDiv ]
    , [ Op InfixL TPlus
      , Op InfixL TMinus ]
    , [ Op Prefix TNot ]
    , [ Op InfixR TUpdate ]
    , [ Op InfixN TLess
      , Op InfixN TGreater
      , Op InfixN TLessEqual
      , Op InfixN TGreaterEqual ]
    , [ Op InfixN TEqual
      , Op InfixN TUnequal ]
    , [ Op InfixL TAnd ]
    , [ Op InfixL TOr ]
    , [ Op InfixL TImplies ]
    ]

tokenText :: Token -> Text
tokenText (Identifier i)     = i
tokenText (Integer i)        = pack (show i)
tokenText (Float f)          = pack (show f)
tokenText (EnvPath p)        = "<" <> p <> ">"

tokenText KAssert            = "assert"
tokenText KElse              = "else"
tokenText KIf                = "if"
tokenText KIn                = "in"
tokenText KInherit           = "inherit"
tokenText KLet               = "let"
tokenText KOr                = "or"
tokenText KRec               = "rec"
tokenText KThen              = "then"
tokenText KWith              = "with"

tokenText TBraceOpen         = "{"
tokenText TBraceClose        = "}"
tokenText TBrackOpen         = "["
tokenText TBrackClose        = "]"
tokenText TInterOpen         = "${"
tokenText TInterClose        = "}"
tokenText TParenOpen         = "("
tokenText TParenClose        = ")"

tokenText TAssign            = "="
tokenText TAt                = "@"
tokenText TColon             = ":"
tokenText TComma             = ","
tokenText TDot               = "."
tokenText TDoubleQuote       = "\""
tokenText TDoubleSingleQuote = "''"
tokenText TEllipsis          = "..."
tokenText TQuestion          = "?"
tokenText TSemicolon         = ";"

tokenText TPlus              = "+"
tokenText TMinus             = "-"
tokenText TMul               = "*"
tokenText TDiv               = "/"
tokenText TConcat            = "++"
tokenText TNegate            = "-"
tokenText TUpdate            = "//"

tokenText TAnd               = "&&"
tokenText TOr                = "||"
tokenText TEqual             = "=="
tokenText TGreater           = ">"
tokenText TGreaterEqual      = ">="
tokenText TImplies           = "->"
tokenText TLess              = "<"
tokenText TLessEqual         = "<="
tokenText TNot               = "!"
tokenText TUnequal           = "!="

tokenText SOF                = ""
