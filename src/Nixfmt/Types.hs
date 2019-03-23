module Nixfmt.Types where

import           Data.Text       hiding (concat, map)
import           Data.Void
import           Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

data Trivium = EmptyLine
             | TrailingComment Text
             | LineComment     Text
             | BlockComment    [Text]
             deriving (Show)

type Trivia = [Trivium]

data AST n l = Node n [AST n l]
             | Leaf l

type NixAST = AST NodeType NixToken

data NodeType
    = Abstraction
    | SetAbstraction
    | Apply
    | Assert
    | File
    | IfElse
    | Inherit
    | InheritFrom
    | List
    deriving (Show)

data NixToken
    = EnvPath    Text
    | Identifier Text
    | NixFloat   Text
    | NixInt     Int
    | NixText    Text
    | NixURI     Text
    | Trivia     Trivia

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

instance (Show n, Show l) => Show (AST n l) where
    show (Leaf l) = show l
    show (Node n xs) = concat
        [ show n
        , "("
        , concat $ map show xs
        , ")"
        ]

instance Show NixToken where
    show (Identifier s) = show s
    show (EnvPath p)    = show p
    show (NixFloat f)   = show f
    show (NixInt i)     = show i
    show (NixURI uri)   = show uri
    show (NixText text) = show text
    show (Trivia ts)    = show ts

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

    show TEOF           = ""
