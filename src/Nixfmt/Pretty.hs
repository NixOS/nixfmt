{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Nixfmt.Pretty where

import Prelude hiding (String)

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Text as Text
  (dropEnd, empty, init, isInfixOf, last, null, strip, takeWhile)

import Nixfmt.Predoc
  (Doc, Pretty, base, emptyline, group, hardline, hardspace, hcat, line, line',
  nest, newline, pretty, sepBy, softline, softline', text, textWidth)
import Nixfmt.Types
  (Ann(..), Binder(..), Expression(..), Item(..), Items(..), Leaf,
  ParamAttr(..), Parameter(..), Selector(..), SimpleSelector(..),
  StringPart(..), Term(..), Token(..), TrailingComment(..), Trivia, Trivium(..),
  Whole(..), tokenText)
import Nixfmt.Util (commonIndentation, isSpaces, replaceMultiple)

prettyCommentLine :: Text -> Doc
prettyCommentLine l
    | Text.null l = emptyline
    | otherwise   = text l <> hardline

toLineComment :: Text -> Trivium
toLineComment c = LineComment $ fromMaybe (" " <> c) $ stripPrefix "*" c

-- Make sure a group is not expanded because the token that starts it has
-- leading comments.
groupWithStart :: Pretty a => Ann a -> Doc -> Doc
groupWithStart (Ann leading a trailing) b
    = pretty leading <> group (pretty a <> pretty trailing <> b)

instance Pretty TrailingComment where
    pretty (TrailingComment c)
        = hardspace <> text "#" <> hardspace <> text c <> hardline

instance Pretty Trivium where
    pretty EmptyLine        = emptyline
    pretty (LineComment c)  = text "#" <> pretty c <> hardline
    pretty (BlockComment c)
        | all ("*" `isPrefixOf`) (tail c) = hcat (map toLineComment c)
        | otherwise
            = base $ text "/*" <> hardspace
              <> nest 3 (hcat (map prettyCommentLine c))
              <> text "*/" <> hardline

prettyItems :: Pretty a => Doc -> Items a -> Doc
prettyItems sep = prettyItems' . unItems
  where
    prettyItems' :: Pretty a => [Item a] -> Doc
    prettyItems' [] = mempty
    prettyItems' [DetachedComments trivia] = pretty trivia
    prettyItems' [CommentedItem trivia x]  = pretty trivia <> group x
    prettyItems' (DetachedComments trivia : xs)
        = pretty trivia <> emptyline <> prettyItems' xs
    prettyItems' (CommentedItem trivia x : xs)
        = pretty trivia <> group x <> sep <> prettyItems' xs

instance Pretty [Trivium] where
    pretty []     = mempty
    pretty trivia = hardline <> hcat trivia

instance Pretty a => Pretty (Ann a) where
    pretty (Ann leading x trailing)
        = pretty leading <> pretty x <> pretty trailing

instance Pretty SimpleSelector where
    pretty (IDSelector i)              = pretty i
    pretty (InterpolSelector interpol) = pretty interpol
    pretty (StringSelector (Ann leading s trailing))
        = pretty leading <> prettySimpleString s <> pretty trailing

instance Pretty Selector where
    pretty (Selector dot sel Nothing)
        = pretty dot <> pretty sel

    pretty (Selector dot sel (Just (kw, def)))
        = pretty dot <> pretty sel
          <> hardspace <> pretty kw <> hardspace <> pretty def

instance Pretty Binder where
    pretty (Inherit inherit Nothing ids semicolon)
        = base $ pretty inherit <> softline
                 <> nest 2 (sepBy softline ids) <> pretty semicolon

    pretty (Inherit inherit (Just source) ids semicolon)
        = base $ pretty inherit <> hardspace
                 <> pretty source <> line
                 <> nest 2 (sepBy softline ids) <> pretty semicolon

    pretty (Assignment selectors assign expr semicolon)
        = base $ group $ hcat selectors <> hardspace <> nest 2 value
        where
            value = pretty assign <> softline <> pretty expr <> pretty semicolon

-- | Pretty print a term without wrapping it in a group.
prettyTerm :: Term -> Doc
prettyTerm (Token t) = pretty t
prettyTerm (String s) = pretty s
prettyTerm (Path p) = pretty p
prettyTerm (Selection term selectors) = pretty term <> hcat selectors

prettyTerm (List (Ann leading paropen Nothing) (Items []) (Ann [] parclose trailing))
    = pretty leading <> pretty paropen <> hardspace <> pretty parclose <> pretty trailing

prettyTerm (List (Ann leading paropen Nothing) (Items [CommentedItem [] item]) (Ann [] parclose trailing))
    | isAbsorbable item
        = pretty leading <> pretty paropen <> pretty item <> pretty parclose <> pretty trailing

prettyTerm (List (Ann [] paropen trailing) items parclose)
    = base $ pretty paropen <> pretty trailing <> line
        <> nest 2 (prettyItems line items) <> line
        <> pretty parclose

-- Lists with leading comments get their own group so the comments don't always
-- force the list to be split over multiple lines.
prettyTerm (List paropen items parclose)
    = base $ groupWithStart paropen $
        line
        <> nest 2 (prettyItems line items) <> line
        <> pretty parclose

prettyTerm (Set Nothing (Ann [] paropen Nothing) (Items []) parclose)
    = pretty paropen <> hardspace <> pretty parclose

prettyTerm (Set krec paropen binders parclose)
    = base $ pretty (fmap ((<>hardspace) . pretty) krec)
        <> pretty paropen <> line
        <> nest 2 (prettyItems hardline binders) <> line
        <> pretty parclose

prettyTerm (Parenthesized paropen expr parclose)
    = base $ pretty paropen <> nest 2 (group expr) <> pretty parclose

instance Pretty Term where
    pretty l@List{} = group $ prettyTerm l
    pretty x        = prettyTerm x

toLeading :: Maybe TrailingComment -> Trivia
toLeading Nothing = []
toLeading (Just (TrailingComment c)) = [LineComment (" " <> c)]

prettyComma :: Maybe Leaf -> Doc
prettyComma Nothing = mempty
prettyComma (Just comma) = softline' <> pretty comma <> hardspace

instance Pretty ParamAttr where
    pretty (ParamAttr name Nothing comma)
        = pretty name <> prettyComma comma

    pretty (ParamAttr name (Just (qmark, def)) comma)
        = group (pretty name <> hardspace <> pretty qmark
            <> absorb softline mempty (Just 2) def)
            <> prettyComma comma

    pretty (ParamEllipsis ellipsis)
        = pretty ellipsis

instance Pretty Parameter where
    pretty (IDParameter i) = pretty i
    pretty (SetParameter bopen attrs bclose)
        = groupWithStart bopen $
            hardspace
            <> hcat attrs <> softline
            <> pretty bclose

    pretty (ContextParameter param1 at param2)
        = pretty param1 <> pretty at <> pretty param2

isAbsorbable :: Term -> Bool
isAbsorbable (String (Ann _ parts@(_:_:_) _))
    = not $ isSimpleString parts
isAbsorbable (Set _ _ (Items (_:_)) _)                                   = True
isAbsorbable (List (Ann [] _ Nothing) (Items [CommentedItem [] item]) _) = isAbsorbable item
isAbsorbable (Parenthesized (Ann [] _ Nothing) (Term t) _)               = isAbsorbable t
isAbsorbable (List _ (Items (_:_:_)) _)                                  = True
isAbsorbable _                                                           = False

absorb :: Doc -> Doc -> Maybe Int -> Expression -> Doc
absorb left right _ (Term t)
    | isAbsorbable t = toHardspace left <> prettyTerm t <> toHardspace right
    where toHardspace x | x == mempty    = mempty
                        | x == softline' = mempty
                        | x == line'     = mempty
                        | otherwise      = hardspace

absorb left right Nothing x = left <> pretty x <> right
absorb left right (Just level) x
    = left <> nest level (pretty x) <> right

absorbSet :: Expression -> Doc
absorbSet = absorb line mempty Nothing

absorbThen :: Expression -> Doc
absorbThen (Term t) | isAbsorbable t = hardspace <> prettyTerm t <> hardspace
absorbThen x                         = line <> nest 2 (group x) <> line

absorbElse :: Expression -> Doc
absorbElse (If if_ cond then_ expr0 else_ expr1)
    = hardspace <> pretty if_ <> hardspace <> group cond <> hardspace
      <> pretty then_ <> absorbThen expr0
      <> pretty else_ <> absorbElse expr1

absorbElse (Term t) | isAbsorbable t = hardspace <> prettyTerm t
absorbElse x                         = line <> nest 2 (group x)

absorbApp :: Expression -> Doc
absorbApp (Application f x) = softline <> pretty f <> absorbApp x
absorbApp (Term t) | isAbsorbable t = hardspace <> group (prettyTerm t)
absorbApp x = softline <> pretty x

instance Pretty Expression where
    pretty (Term t) = pretty t

    pretty (With with expr0 semicolon expr1)
        = base (pretty with <> hardspace
          <> nest 2 (group expr0) <> pretty semicolon)
          <> absorbSet expr1

    pretty (Let let_ (Items []) in_ expr)
        = base $ pretty let_ <> hardspace <> pretty in_ <> hardspace <> pretty expr

    pretty (Let let_ (Items [CommentedItem [] item]) in_ expr)
        = base $ letPart <> line <> inPart
        where letPart = groupWithStart let_ $ line <> nest 2 (pretty item)
              inPart = groupWithStart in_ $ hardspace <> pretty expr

    pretty (Let let_ binders in_ expr)
        = base $ letPart <> emptyline <> inPart
        where letPart = groupWithStart let_ $ line <> letBody
              inPart = groupWithStart in_ $ hardspace <> pretty expr
              letBody = nest 2 $ prettyItems hardline binders

    pretty (Assert assert cond semicolon expr)
        = base (pretty assert <> hardspace
          <> nest 2 (group cond) <> pretty semicolon)
          <> absorbSet expr

    pretty (If if_ cond then_ expr0 else_ expr1)
        = base $ groupWithStart if_ $
            hardspace <> group cond <> hardspace
            <> pretty then_ <> absorbThen expr0
            <> pretty else_ <> absorbElse expr1

    pretty (Abstraction (IDParameter param) colon body)
        = pretty param <> pretty colon <> absorbAbs body
        where absorbAbs (Abstraction (IDParameter param0) colon0 body0) =
                  hardspace <> pretty param0 <> pretty colon0 <> absorbAbs body0
              absorbAbs x = absorbSet x

    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> absorbSet body

    pretty (Application f x) = group $ pretty f <> absorbApp x

    pretty (Operation a op b)
        = pretty a <> softline
          <> pretty op <> hardspace <> pretty b

    pretty (MemberCheck expr qmark sel)
        = pretty expr <> softline
          <> pretty qmark <> hardspace <> hcat sel

    pretty (Negation minus expr)
        = pretty minus <> pretty expr

    pretty (Inversion bang expr)
        = pretty bang <> pretty expr

instance Pretty a => Pretty (Whole a) where
    pretty (Whole x finalTrivia)
        = group $ pretty x <> pretty finalTrivia

instance Pretty Token where
    pretty = text . tokenText

-- STRINGS

isSimpleSelector :: Selector -> Bool
isSimpleSelector (Selector _ (IDSelector _) Nothing) = True
isSimpleSelector _                                   = False

isSimple :: Expression -> Bool
isSimple (Term (Token (Ann [] (Identifier _) Nothing))) = True
isSimple (Term (Selection t selectors))
    = isSimple (Term t) && all isSimpleSelector selectors
isSimple _ = False

hasQuotes :: [StringPart] -> Bool
hasQuotes []                = False
hasQuotes (TextPart x : xs) = Text.isInfixOf "\"" x || hasQuotes xs
hasQuotes (_ : xs)          = hasQuotes xs

hasDualQuotes :: [StringPart] -> Bool
hasDualQuotes []                = False
hasDualQuotes (TextPart x : xs) = Text.isInfixOf "''" x || hasDualQuotes xs
hasDualQuotes (_ : xs)          = hasDualQuotes xs

endsInSingleQuote :: [StringPart] -> Bool
endsInSingleQuote []           = False
endsInSingleQuote xs =
    case last xs of
         (TextPart x) -> x /= Text.empty && Text.last x == '\''
         _            -> False

isIndented :: [[StringPart]] -> Bool
isIndented parts =
    case commonIndentation inits of
         Just "" -> False
         _       -> True
    where textInit (TextPart t : xs) = t <> textInit xs
          textInit _                 = ""
          nonEmpty (TextPart "" : xs) = nonEmpty xs
          nonEmpty []                 = False
          nonEmpty _                  = True
          inits = map textInit $ filter nonEmpty parts

-- | If the last line has at least one space but nothing else, it cannot be
-- cleanly represented in an indented string.
lastLineIsSpaces :: [[StringPart]] -> Bool
lastLineIsSpaces [] = False
lastLineIsSpaces xs = case last xs of
    [TextPart t] -> isSpaces t
    _            -> False

isInvisibleLine :: [StringPart] -> Bool
isInvisibleLine []           = True
isInvisibleLine [TextPart t] = Text.null $ Text.strip t
isInvisibleLine _            = False

isSimpleString :: [[StringPart]] -> Bool
isSimpleString [parts]
    | hasDualQuotes parts       = True
    | endsInSingleQuote parts   = True
    | isIndented [parts]        = True
    | hasQuotes parts           = False
    | otherwise                 = True

isSimpleString parts
    | all isInvisibleLine parts = True
    | isIndented parts          = True
    | lastLineIsSpaces parts    = True
    | otherwise                 = False

instance Pretty StringPart where
    pretty (TextPart t) = text t
    pretty (Interpolation (Whole (Term t) []))
        | isAbsorbable t
            = group $ text "${" <> prettyTerm t <> text "}"

    pretty (Interpolation (Whole expr []))
        | isSimple expr
            = text "${" <> pretty expr <> text "}"

    pretty (Interpolation whole)
        = group $ text "${" <> line'
            <> nest 2 (pretty whole) <> line'
            <> text "}"

instance Pretty [StringPart] where
    pretty [Interpolation expr]
        = group $ text "${" <> pretty expr <> text "}"

    -- If we split a string line over multiple code lines due to large
    -- interpolations, make sure to indent based on the indentation of the line
    -- in the string.
    pretty (TextPart t : parts)
        = text t <> nest indentation (hcat parts)
        where indentation = textWidth $ Text.takeWhile isSpace t

    pretty parts = hcat parts

instance Pretty [[StringPart]] where
    pretty parts
        | isSimpleString parts = prettySimpleString parts
        | otherwise            = prettyIndentedString parts

type UnescapeInterpol = Text -> Text
type EscapeText = Text -> Text

prettyLine :: EscapeText -> UnescapeInterpol -> [StringPart] -> Doc
prettyLine escapeText unescapeInterpol
    = pretty . unescapeInterpols . map escape
    where escape (TextPart t) = TextPart (escapeText t)
          escape x            = x

          unescapeInterpols [] = []
          unescapeInterpols (TextPart t : TextPart u : xs)
              = unescapeInterpols (TextPart (t <> u) : xs)
          unescapeInterpols (TextPart t : xs@(Interpolation{} : _))
              = TextPart (unescapeInterpol t) : unescapeInterpols xs
          unescapeInterpols (x : xs) = x : unescapeInterpols xs

prettySimpleString :: [[StringPart]] -> Doc
prettySimpleString parts = group $
    text "\""
    <> sepBy (text "\\n") (map (prettyLine escape unescapeInterpol) parts)
    <> text "\""
    where escape = replaceMultiple
              [ ("$\\${", "$${")
              , ("${",    "\\${")
              , ("\"",    "\\\"")
              , ("\r",    "\\r")
              , ("\\",    "\\\\")
              ]

          unescapeInterpol t
              | "$" `isSuffixOf` t = Text.init t <> "\\$"
              | otherwise          = t

prettyIndentedString :: [[StringPart]] -> Doc
prettyIndentedString parts = group $ base $
    text "''" <> line'
    <> nest 2 (sepBy newline (map (prettyLine escape unescapeInterpol) parts))
    <> text "''"
    where escape = replaceMultiple
              [ ("'${", "''\\'''${")
              , ("${", "''${")
              , ("''", "'''")
              ]

          unescapeInterpol t
              | Text.null t        = t
              | Text.last t /= '$' = t
              | trailingQuotes (Text.init t) `mod` 3 == 0
                  = Text.init t <> "''$"
              | trailingQuotes (Text.init t) `mod` 3 == 1
                  = Text.dropEnd 2 t <> "''\\'''$"
              | otherwise
                  = error "should never happen after escape"

          trailingQuotes t
              | "'" `isSuffixOf` t = 1 + trailingQuotes (Text.init t)
              | otherwise          = 0 :: Int
