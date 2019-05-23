{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings #-}

module Nixfmt.Pretty where

import Prelude hiding (String)

import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, stripPrefix)
import qualified Data.Text as Text
  (empty, isInfixOf, last, null, replace, strip)

import Nixfmt.Predoc
  (Doc, Pretty, emptyline, group, hardline, hardspace, hcat, line, line', nest,
  newline, pretty, sepBy, softline, softline', text)
import Nixfmt.Types
  (Ann(..), Binder(..), Expression(..), File(..), Leaf, ListPart(..),
  ParamAttr(..), Parameter(..), Selector(..), SimpleSelector(..),
  StringPart(..), Term(..), Token(..), TrailingComment(..), Trivia,
  Trivium(..), tokenText)

prettyCommentLine :: Text -> Doc
prettyCommentLine l
    | Text.null l = emptyline
    | otherwise   = text l <> hardline

toLineComment :: Text -> Trivium
toLineComment c = LineComment $ fromMaybe (" " <> c) $ stripPrefix "*" c

instance Pretty TrailingComment where
    pretty (TrailingComment c)
        = hardspace <> text "#" <> hardspace <> text c <> hardline

instance Pretty Trivium where
    pretty EmptyLine        = emptyline
    pretty (LineComment c)  = text "#" <> pretty c <> hardline
    pretty (BlockComment c)
        | all ("*" `isPrefixOf`) (tail c) = pretty (map toLineComment c)
        | otherwise
            = text "/*" <> hardspace <> (nest 3 $ hcat $ map prettyCommentLine c)
              <> text "*/" <> hardline

instance Pretty [Trivium] where
    pretty [] = mempty
    pretty trivia = hardline <> hcat trivia

instance Pretty a => Pretty (Ann a) where
    pretty (Ann x trailing leading)
        = pretty x <> pretty trailing <> pretty leading

instance Pretty SimpleSelector where
    pretty (IDSelector i)              = pretty i
    pretty (InterpolSelector interpol) = pretty interpol
    pretty (StringSelector s)          = pretty s

instance Pretty Selector where
    pretty (Selector dot sel Nothing)
        = pretty dot <> pretty sel

    pretty (Selector dot sel (Just (kw, def)))
        = pretty dot <> pretty sel
          <> hardspace <> pretty kw <> hardspace <> pretty def

instance Pretty Binder where
    pretty (Inherit inherit Nothing ids semicolon)
        = group (pretty inherit <> softline
                 <> nest 2 (sepBy softline ids)) <> pretty semicolon

    pretty (Inherit inherit source ids semicolon)
        = group (pretty inherit <> hardspace
                 <> pretty source <> line
                 <> nest 2 (sepBy softline ids)) <> pretty semicolon

    pretty (Assignment selectors assign expr semicolon)
        = group (hcat selectors <> hardspace
            <> pretty assign <> softline
            <> pretty expr) <> pretty semicolon

    pretty (BinderTrivia trivia) = pretty trivia

instance Pretty ListPart where
    pretty (ListItem term)     = pretty term
    pretty (ListTrivia trivia) = pretty trivia

-- | Pretty print a term without wrapping it in a group.
prettyTerm :: Term -> Doc
prettyTerm (Token t) = pretty t
prettyTerm (String s) = pretty s
prettyTerm (Selection term selectors) = pretty term <> hcat selectors

prettyTerm (List (Ann paropen Nothing []) [] parclose)
    = pretty paropen <> pretty parclose

prettyTerm (List (Ann paropen Nothing []) [item] parclose)
    = pretty paropen <> pretty item <> pretty parclose

prettyTerm (List (Ann paropen trailing leading) items parclose)
    = pretty paropen <> pretty trailing <> line
        <> nest 2 (sepBy line items') <> line
        <> pretty parclose
    where items' = case leading of
                        [] -> items
                        _  -> ListTrivia leading : items

prettyTerm (Set Nothing (Ann paropen Nothing []) [] parclose)
    = pretty paropen <> hardspace <> pretty parclose

prettyTerm (Set Nothing (Ann paropen Nothing []) [item] parclose)
    = pretty paropen <> line <> nest 2 (pretty item) <> line <> pretty parclose

prettyTerm (Set krec (Ann paropen trailing leading) binders parclose)
    = pretty (fmap ((<>hardspace) . pretty) krec)
        <> pretty paropen <> pretty trailing <> line
        <> nest 2 (sepBy hardline binders') <> line
        <> pretty parclose
    where binders' = case leading of
                          [] -> binders
                          _  -> BinderTrivia leading : binders

prettyTerm (Parenthesized paropen expr parclose)
    = pretty paropen <> group expr <> pretty parclose

instance Pretty Term where
    pretty l@(List _ _ _)    = group $ prettyTerm l
    pretty x                 = prettyTerm x

toLeading :: Maybe TrailingComment -> Trivia
toLeading Nothing = []
toLeading (Just (TrailingComment c)) = [LineComment (" " <> c)]

prettyComma :: Maybe Leaf -> Doc
prettyComma Nothing = mempty
prettyComma (Just (Ann c trailing leading))
    = pretty (toLeading trailing ++ leading)
      <> softline' <> pretty c <> hardspace

instance Pretty ParamAttr where
    pretty (ParamAttr name Nothing comma)
        = pretty name <> prettyComma comma

    pretty (ParamAttr name (Just (qmark, def)) comma)
        = group (pretty name <> hardspace <> pretty qmark
            <> absorb softline mempty 2 def)
            <> prettyComma comma

    pretty (ParamEllipsis ellipsis)
        = pretty ellipsis

instance Pretty Parameter where
    pretty (IDParameter i) = pretty i
    pretty (SetParameter bopen attrs bclose)
        = group $ pretty bopen <> hardspace
                  <> hcat attrs <> softline
                  <> pretty bclose

    pretty (ContextParameter param1 at param2)
        = pretty param1 <> pretty at <> pretty param2

isAbsorbable :: Term -> Bool
isAbsorbable (String (Ann (_:_:_) _ _))                  = True
isAbsorbable (Set _ _ (_:_) _)                           = True
isAbsorbable (List (Ann _ Nothing []) [ListItem item] _) = isAbsorbable item
isAbsorbable (List _ (_:_:_) _)                          = True
isAbsorbable _                                           = False

absorb :: Doc -> Doc -> Int -> Expression -> Doc
absorb left right _ (Term t)
    | isAbsorbable t = toHardspace left <> prettyTerm t <> toHardspace right
    where toHardspace x | x == mempty    = mempty
                        | x == softline' = mempty
                        | x == line'     = mempty
                        | otherwise      = hardspace

absorb left right level x = left <> nest level (pretty x) <> right

absorbSet :: Expression -> Doc
absorbSet = absorb line mempty 0

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

instance Pretty Expression where
    pretty (Term t) = pretty t

    pretty (With with expr0 semicolon expr1)
        = pretty with <> hardspace
          <> pretty expr0 <> pretty semicolon
          <> absorbSet expr1

    pretty (Let (Ann let_ letTrailing letLeading) binders
                (Ann in_ inTrailing inLeading) expr)
        = group $ group (pretty let_ <> pretty letTrailing <> line
                         <> nest 2 (sepBy hardline binders')) <> line
          <> pretty inTrailing <> pretty inLeading
          <> pretty in_ <> hardspace <> group expr
        where binders' = case letLeading of
                              [] -> binders
                              _  -> BinderTrivia letLeading : binders

    pretty (Assert assert cond semicolon expr)
        = pretty assert <> hardspace
          <> pretty cond <> pretty semicolon <> line
          <> pretty expr

    pretty (If if_ cond then_ expr0 else_ expr1)
        = group $ pretty if_ <> hardspace <> group cond <> hardspace
                  <> pretty then_ <> absorbThen expr0
                  <> pretty else_ <> absorbElse expr1

    pretty (Abstraction (IDParameter param) colon body)
        = pretty param <> pretty colon <> absorbAbs body
        where absorbAbs (Abstraction (IDParameter param0) colon0 body0) =
                  hardspace <> pretty param0 <> pretty colon0 <> absorbAbs body0
              absorbAbs x = absorbSet x

    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> absorbSet body

    pretty (Application f (Term t))
        | isAbsorbable t     = pretty f <> hardspace <> group (prettyTerm t)
    pretty (Application f x) = pretty f <> softline <> pretty x

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

instance Pretty File where
    pretty (File (Ann _ Nothing leading) expr)
        = group $ hcat leading <> pretty expr <> hardline

    pretty (File (Ann _ (Just (TrailingComment trailing)) leading) expr)
        = group $ text "# " <> pretty trailing <> hardline
                  <> hcat leading <> pretty expr <> hardline

instance Pretty Token where
    pretty = text . tokenText

instance Pretty [Token] where
    pretty = hcat

-- STRINGS

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

isEmptyLine :: [StringPart] -> Bool
isEmptyLine []           = True
isEmptyLine [TextPart t] = Text.strip t == Text.empty
isEmptyLine _            = False

instance Pretty StringPart where
    pretty (TextPart t) = text t
    pretty (Interpolation paropen (Term t) parclose)
        | isAbsorbable t
            = group $ pretty paropen <> prettyTerm t <> pretty parclose

    pretty (Interpolation paropen expr parclose)
        = group $ pretty paropen <> line'
            <> nest 2 (pretty expr) <> line'
            <> pretty parclose

instance Pretty [[StringPart]] where
    pretty s = prettyString s

prettyString :: [[StringPart]] -> Doc
prettyString [parts]
    | hasDualQuotes parts || endsInSingleQuote parts
                      = prettySimpleString [parts]
    | hasQuotes parts = prettyIndentedString [parts]
    | otherwise       = prettySimpleString [parts]

prettyString parts
    | all isEmptyLine parts = prettySimpleString parts
    | otherwise             = prettyIndentedString parts

prettyLine :: [StringPart] -> Doc
prettyLine [Interpolation paropen expr parclose]
    = group $ pretty paropen <> pretty expr <> pretty parclose

prettyLine (TextPart t : parts)
    | Text.null (Text.strip t) = text t <> prettyLine parts

prettyLine parts = hcat parts

prettySimpleString :: [[StringPart]] -> Doc
prettySimpleString parts = group $
    text "\""
    <> (sepBy (text "\\n") (map (prettyLine . map escape) parts))
    <> text "\""
    where escape (TextPart t) = TextPart
              $ Text.replace "$\\${" "$${"
              $ Text.replace "${" "\\${"
              $ Text.replace "\"" "\\\""
              $ Text.replace "\\" "\\\\" t
          escape x            = x

prettyIndentedString :: [[StringPart]] -> Doc
prettyIndentedString parts = group $
    text "''" <> line'
    <> nest 2 (sepBy newline (map (prettyLine . map escape) parts))
    <> text "''"
    where escape (TextPart t) = TextPart
              $ Text.replace "$''${" "$${"
              $ Text.replace "${" "''${"
              $ Text.replace "''" "'''" t
          escape x            = x
