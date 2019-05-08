{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings #-}

module Nixfmt.Pretty where

import Prelude hiding (String)

import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, stripPrefix)
import qualified Data.Text as Text

import Nixfmt.Predoc
import Nixfmt.Types
import Nixfmt.Util

prettyCommentLine :: Text -> Doc
prettyCommentLine l
    | Text.null l = emptyline
    | otherwise   = text l <> hardline

toLineComment :: Text -> Trivium
toLineComment c = LineComment $ fromMaybe c $ stripPrefix "*" c

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

    pretty (Assignment selectors assign
            (Term (String s@(Ann [[TextPart t]] trailing leading))) semicolon)
        | validURI t            = group $
            hcat selectors <> hardspace <> pretty assign
            <> line <> nest 2 (pretty t) <> pretty semicolon
            <> pretty trailing <> pretty leading
        | otherwise             = group $
            hcat selectors <> hardspace <> pretty assign
            <> line <> nest 2 (pretty s) <> pretty semicolon

    pretty (Assignment selectors assign expr semicolon)
        = group $ hcat selectors <> hardspace
                  <> pretty assign <> softline
                  <> group (pretty expr <> pretty semicolon)


    pretty (BinderTrivia trivia) = pretty trivia

instance Pretty ListPart where
    pretty (ListItem term)     = pretty term
    pretty (ListTrivia trivia) = pretty trivia

instance Pretty Term where
    pretty (Token x)  = pretty x
    pretty (String x) = pretty x

    pretty (List (Ann paropen Nothing []) [] parclose)
        = pretty paropen <> hardspace <> pretty parclose

    pretty (List (Ann paropen trailing leading) items parclose)
        = group $ pretty paropen <> pretty trailing <> line
                  <> nest 2 (sepBy line items') <> line
                  <> pretty parclose
        where items' = case leading of
                            [] -> items
                            _  -> ListTrivia leading : items

    pretty (Set krec (Ann paropen Nothing []) [] parclose)
        = pretty (fmap ((<>hardspace) . pretty) krec)
          <> pretty paropen <> hardspace <> pretty parclose

    pretty (Set krec (Ann paropen trailing leading) binders parclose)
        = group $ pretty (fmap ((<>hardspace) . pretty) krec)
                  <> pretty paropen <> pretty trailing <> line
                  <> nest 2 (sepBy hardline binders') <> line
                  <> pretty parclose
        where binders' = case leading of
                              [] -> binders
                              _  -> BinderTrivia leading : binders

    pretty (Selection term selectors)
        = group $ pretty term <> line' <> sepBy line' selectors

    pretty (Parenthesized paropen expr parclose)
        = pretty paropen <> group (pretty expr) <> pretty parclose

toLeading :: Maybe TrailingComment -> Trivia
toLeading Nothing = []
toLeading (Just (TrailingComment c)) = [LineComment c]

prettyComma :: Maybe Leaf -> Doc
prettyComma Nothing = mempty
prettyComma (Just (Ann c trailing leading))
    = pretty (toLeading trailing ++ leading)
      <> softline' <> pretty c <> hardspace

instance Pretty ParamAttr where
    pretty (ParamAttr name Nothing comma)
        = pretty name <> prettyComma comma

    pretty (ParamAttr name (Just (qmark, def)) comma)
        = pretty name <> softline
          <> nest 2 (pretty qmark <> hardspace <> pretty def)
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

absorbSet :: Expression -> Doc
absorbSet set@(Term (Set _ _ _ _)) = hardspace <> pretty set
absorbSet l@(Term (List _ _ _)) = hardspace <> pretty l
absorbSet s@(Term (String _)) = hardspace <> pretty s
absorbSet x = line <> pretty x

absorbThen :: Expression -> Doc
absorbThen set@(Term (Set _ _ _ _)) = hardspace <> pretty set <> hardspace
absorbThen l@(Term (List _ _ _))    = hardspace <> pretty l <> hardspace
absorbThen s@(Term (String _))      = hardspace <> pretty s <> hardspace
absorbThen x                        = line <> nest 2 (pretty x) <> line

absorbElse :: Expression -> Doc
absorbElse set@(Term (Set _ _ _ _)) = hardspace <> pretty set
absorbElse l@(Term (List _ _ _))    = hardspace <> pretty l
absorbElse s@(Term (String _))      = hardspace <> pretty s

absorbElse (If if_ cond then_ expr0 else_ expr1)
    = hardspace <> pretty if_ <> hardspace <> pretty cond <> hardspace
      <> pretty then_ <> absorbThen expr0
      <> pretty else_ <> absorbElse expr1

absorbElse x                        = line <> nest 2 (pretty x)

instance Pretty Expression where
    pretty (Term t) = pretty t

    pretty (With with expr0 semicolon expr1)
        = pretty with <> hardspace
          <> pretty expr0 <> pretty semicolon
          <> absorbSet expr1

    pretty (Let (Ann let_ letTrailing letLeading) binders
                (Ann in_ inTrailing inLeading) expr)
        = group (pretty let_ <> pretty letTrailing <> line
                 <> nest 2 (sepBy hardline binders')) <> hardline
          <> pretty inTrailing <> pretty inLeading
          <> pretty in_ <> hardspace <> pretty expr
        where binders' = case letLeading of
                              [] -> binders
                              _  -> BinderTrivia letLeading : binders

    pretty (Assert assert cond semicolon expr)
        = pretty assert <> hardspace
          <> pretty cond <> pretty semicolon <> line
          <> pretty expr

    pretty (If if_ cond then_ expr0 else_ expr1)
        = group $ pretty if_ <> hardspace <> pretty cond <> hardspace
                  <> pretty then_ <> absorbThen expr0
                  <> pretty else_ <> absorbElse expr1

    pretty (Abstraction (IDParameter param) colon body)
        = group $ pretty param <> pretty colon <> absorb body
        where absorb (Abstraction (IDParameter param0) colon0 body0) =
                  hardspace <> pretty param0 <> pretty colon0 <> absorb body0
              absorb x = absorbSet x

    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> absorbSet body

    pretty (Application f x@(Term (List _ _ _)))
        = pretty f <> hardspace <> pretty x

    pretty (Application f x@(Term (Set _ _ _ _)))
        = pretty f <> hardspace <> pretty x

    pretty (Application f x)
        = pretty f <> softline <> pretty x

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

validURI :: Text -> Bool
validURI t =
    case Text.splitOn "://" t of
         ["", _]       -> False
         [_, ""]       -> False
         [scheme, uri] -> Text.all schemeChar scheme && Text.all uriChar uri
         _             -> False

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
    pretty (TextPart t) = pretty t
    pretty (Interpolation paropen expr parclose) = group $
        pretty paropen <> line'
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


prettySimpleString :: [[StringPart]] -> Doc
prettySimpleString parts = group $
    text "\""
    <> (sepBy (text "\\n") (map (hcat . map escape) parts))
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
    <> nest 2 (sepBy newline (map (hcat . map escape) parts))
    <> text "''"
    where escape (TextPart t) = TextPart
              $ Text.replace "$''${" "$${"
              $ Text.replace "${" "''${"
              $ Text.replace "''" "'''" t
          escape x            = x
