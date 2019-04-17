{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Types where

import           Prelude         hiding (String)

import           Data.Text       hiding (concat, map)
import           Data.Void
import           Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

data Trivium = EmptyLine
             | LineComment     Text
             | BlockComment    [Text]

type Trivia = [Trivium]

instance Show Trivium where
    show EmptyLine         = "EmptyLine"
    show (LineComment lc)  = " # " <> unpack lc
    show (BlockComment bc) = "/*" <> concat (map unpack bc) <> "*/"

    showList []     tail = tail
    showList (x:xs) tail = show x ++ (showList xs tail)

data Ann a = Ann a (Maybe Text) Trivia

instance Show a => Show (Ann a) where
    show (Ann x Nothing leading) = show x <> show leading
    show (Ann x (Just trailing) leading) =
        show x <> show ("/*" <> trailing <> "*/") <> show leading

type Leaf = Ann Token

data StringPart = TextPart Text
                | Interpolation Leaf Expression Token
                deriving (Show)

data String = SimpleString Token [StringPart] Leaf
            | IndentedString Token [StringPart] Leaf
            | URIString (Ann Text)
            deriving (Show)

data SimpleSelector = IDSelector Leaf
                    | InterpolSelector StringPart
                    | StringSelector String
                    deriving (Show)

data Selector = Selector (Maybe Leaf) SimpleSelector (Maybe (Leaf, Term))
              deriving (Show)

data Binder = Inherit Leaf (Maybe Term) [Leaf] Leaf
            | Assignment [Selector] Leaf Expression Leaf
            | BinderTrivia Trivia
            deriving (Show)

data ListPart = ListItem Term
              | ListTrivia Trivia
              deriving (Show)

data Term = Token Leaf
          | String String
          | List Leaf [ListPart] Leaf
          | Set (Maybe Leaf) Leaf [Binder] Leaf
          | Selection Term [Selector]
          | Parenthesized Leaf Expression Leaf
          deriving (Show)

data ParamAttr = ParamAttr Leaf (Maybe (Leaf, Expression)) (Maybe Leaf)
               | ParamEllipsis Leaf
               deriving (Show)

data Parameter = IDParameter Leaf
               | SetParameter Leaf [ParamAttr] Leaf
               | ContextParameter Parameter Leaf Parameter
               deriving (Show)

data Expression = Term Term
                | With Leaf Expression Leaf Expression
                | Let Leaf [Binder] Leaf Expression
                | Assert Leaf Expression Leaf Expression
                | If Leaf Expression Leaf Expression Leaf Expression
                | Abstraction Parameter Leaf Expression

                | Application Expression Expression
                | Operation Expression Leaf Expression
                | MemberCheck Expression Leaf Selector
                | Negation Leaf Expression
                | Inversion Leaf Expression
                deriving (Show)

data File = File Leaf Expression
          deriving (Show)

data Token = Integer    Int
           | Identifier Text
           | Path       Text
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
           deriving (Eq)


data Fixity = Prefix
            | InfixL
            | InfixN
            | InfixR
            | Postfix
            deriving (Show)

data Operator = Op Fixity Token
              | Apply
              deriving (Show)

operators :: [[Operator]]
operators = [ [ Apply ]
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

instance Show Token where
    show (Identifier i)     = unpack i
    show (Integer i)        = show i

    show KAssert            = "assert"
    show KElse              = "else"
    show KIf                = "if"
    show KIn                = "in"
    show KInherit           = "inherit"
    show KLet               = "let"
    show KOr                = "or"
    show KRec               = "rec"
    show KThen              = "then"
    show KWith              = "with"

    show TBraceOpen         = "{"
    show TBraceClose        = "}"
    show TBrackOpen         = "["
    show TBrackClose        = "]"
    show TInterOpen         = "${"
    show TInterClose        = "}"
    show TParenOpen         = "("
    show TParenClose        = ")"

    show TAssign            = "="
    show TAt                = "@"
    show TColon             = ":"
    show TComma             = ","
    show TDot               = "."
    show TDoubleQuote       = "\""
    show TDoubleSingleQuote = "''"
    show TEllipsis          = "..."
    show TQuestion          = "?"
    show TSemicolon         = ";"

    show TPlus              = "+"
    show TMinus             = "-"
    show TMul               = "*"
    show TDiv               = "/"
    show TConcat            = "++"
    show TUpdate            = "//"

    show TAnd               = "&&"
    show TOr                = "||"
    show TEqual             = "=="
    show TGreater           = ">"
    show TGreaterEqual      = ">="
    show TImplies           = "->"
    show TLess              = "<"
    show TLessEqual         = "<="
    show TNot               = "!"
    show TUnequal           = "!="

    show SOF                = ""
