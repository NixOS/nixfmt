module Nixfmt.Types where

import           Data.Text
import           Text.Megaparsec (SourcePos)

data Ann e = Ann
    { preTrivia  :: [Trivium]
    , startPos   :: Maybe SourcePos
    , annotated  :: e
    , endPos     :: Maybe SourcePos
    , postTrivia :: [Trivium]
    } deriving (Show)

data AST
    = Node NodeType [Ann AST]
    | Leaf NixToken
    deriving (Show)

data NodeType
    = Abstraction
    | SetAbstraction
    | Apply
    | Assert
    | IfElse
    | Inherit
    | InheritFrom
    | List
    deriving (Show)

data Trivium
    = Newlines     Int
    | Spaces       Int
    | Tabs         Int
    | LineComment  Text
    | BlockComment Text
    deriving (Show)

data NixValue
    = NixFloat Text
    | NixInt   Int
    | NixText  Text
    | NixURI   Text
    deriving (Show)

data NixToken
    = Literal    NixValue
    | Identifier Text
    | EnvPath    Text

    | TAssert
    | TElse
    | TIf
    | TIn
    | TInherit
    | TLet
    | TRec
    | TThen
    | TWith

    | TBraceOpen
    | TBraceClose
    | TBrackOpen
    | TBrackClose
    | TParenOpen
    | TParenClose

    | TAssign
    | TAt
    | TColon
    | TComma
    | TDot
    | TEllipsis
    | TQuestion
    | TSemicolon

    | TConcat
    | TNegate
    | TMerge

    | TAdd
    | TSub
    | TMul
    | TDiv

    | TAnd
    | TEqual
    | TImplies
    | TLess
    | TLessOrEqual
    | TGreater
    | TGreaterOrEqual
    | TNotEqual
    | TOr

    | TEOF

instance Show NixToken where
    show (Literal v)    = show v
    show (Identifier s) = show s
    show (EnvPath p)    = show p

    show TAssert        = "assert"
    show TElse          = "else"
    show TIf            = "if"
    show TIn            = "in"
    show TInherit       = "inherit"
    show TLet           = "let"
    show TRec           = "rec"
    show TThen          = "then"
    show TWith          = "with"

    show TBraceOpen     = "{"
    show TBraceClose    = "}"
    show TBrackOpen     = "["
    show TBrackClose    = "]"
    show TParenOpen     = "("
    show TParenClose    = ")"

    show TComma         = ","
