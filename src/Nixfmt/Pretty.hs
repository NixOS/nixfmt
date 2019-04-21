{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Pretty where

import           Prelude       hiding (String)

import           Data.List     hiding (group)

import           Nixfmt.Predoc
import           Nixfmt.Types

instance Pretty Trivium where
    pretty EmptyLine        = emptyline
    pretty (LineComment c)  = text "#" <> pretty c <> hardline
    pretty (BlockComment c)
        = text "/*" <> sepBy hardline c <> text "*/" <> hardline

instance Pretty [Trivium] where
    pretty [] = mempty
    pretty ts = hardline <> hcat ts

instance Pretty a => Pretty (Ann a) where
    pretty (Ann x Nothing leading)
        = pretty x <> pretty leading

    pretty (Ann x (Just c) leading)
        = pretty x <> text " # " <> text c <> hardline <> pretty leading

instance Pretty StringPart where
    pretty (TextPart t) = pretty t
    pretty (Interpolation paropen expr parclose)
        = group $ pretty paropen <> line
                  <> pretty expr <> line
                  <> pretty parclose

instance Pretty String where
    pretty (SimpleString qopen parts qclose)
        = pretty qopen <> hcat parts <> pretty qclose

    pretty (IndentedString qopen parts qclose)
        = pretty qopen <> hcat parts <> pretty qclose

    pretty (URIString t) = pretty t

instance Pretty SimpleSelector where
    pretty (IDSelector i)              = pretty i
    pretty (InterpolSelector interpol) = pretty interpol
    pretty (StringSelector s)          = pretty s

instance Pretty Selector where
    pretty (Selector dot sel Nothing)
        = pretty dot <> pretty sel

    pretty (Selector dot sel (Just (or, def)))
        = pretty dot <> pretty sel
          <> hardspace <> pretty or <> hardspace <> pretty def

instance Pretty Binder where
    pretty (Inherit inherit source ids semicolon)
        = group $ pretty inherit <> hardspace
                  <> pretty source <> hardspace
                  <> nest 2 (sepBy softline ids) <> pretty semicolon

    pretty (Assignment selectors assign expr semicolon)
        = group $ hcat selectors <> hardspace
                  <> pretty assign <> softline
                  <> pretty expr <> pretty semicolon

    pretty (BinderTrivia ts) = pretty ts

instance Pretty ListPart where
    pretty (ListItem term) = pretty term
    pretty (ListTrivia ts) = pretty ts

instance Pretty Term where
    pretty (Token x)  = pretty x
    pretty (String x) = pretty x

    pretty (List paropen items parclose)
        = group $ pretty paropen <> line
                  <> nest 2 (vsep items) <> line
                  <> pretty parclose

    pretty (Set rec paropen bindings parclose)
        = group $ pretty (fmap ((<>hardspace) . pretty) rec)
                  <> pretty paropen <> line
                  <> nest 2 (vsep bindings) <> line
                  <> pretty parclose

    pretty (Selection term selectors)
        = group $ pretty term <> group (sepBy softline' selectors)

    pretty (Parenthesized paropen expr parclose)
        = pretty paropen <> pretty expr <> pretty parclose

instance Pretty ParamAttr where
    pretty (ParamAttr name Nothing comma)
        = pretty name <> pretty (fmap ((softline<>) . pretty) comma)

    pretty (ParamAttr name (Just (qmark, def)) comma)
        = pretty name <> softline
          <> pretty qmark <> hardspace
          <> pretty def <> pretty (fmap ((softline<>) . pretty) comma)

    pretty (ParamEllipsis ellipsis)
        = pretty ellipsis

instance Pretty Parameter where
    pretty (IDParameter i) = pretty i
    pretty (SetParameter bopen attrs bclose)
        = pretty bopen <> hardspace
          <> sepBy softline attrs <> softline
          <> pretty bclose

    pretty (ContextParameter param1 at param2)
        = pretty param1 <> pretty at <> pretty param2

instance Pretty Expression where
    pretty (Term t) = pretty t

    pretty (With with expr0 semicolon expr1)
        = pretty with <> hardspace
          <> pretty expr0 <> pretty semicolon <> line
          <> pretty expr1

    pretty (Let let_ binders in_ expr)
        = group (pretty let_ <> line
                 <> sepBy hardline binders) <> emptyline
          <> pretty in_ <> hardspace <> pretty expr

    pretty (Assert assert cond semicolon expr)
        = pretty assert <> hardspace
          <> pretty cond <> pretty semicolon <> line
          <> pretty expr

    pretty (If if_ cond then_ expr0 else_ expr1)
        = group (pretty if_ <> hardspace <> pretty cond <> line
                 <> pretty then_ <> hardspace <> pretty expr0 <> line
                 <> pretty else_ <> hardspace <> pretty expr1)

    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> line <> pretty body

    pretty (Application f x)
        = pretty f <> softline <> pretty x

    pretty (Operation a op b)
        = pretty a <> softline
          <> pretty op <> hardspace <> pretty b

    pretty (MemberCheck expr qmark sel)
        = pretty expr <> softline
          <> pretty qmark <> hardspace <> pretty sel

    pretty (Negation minus expr)
        = pretty minus <> pretty expr

    pretty (Inversion bang expr)
        = pretty bang <> pretty expr

instance Pretty File where
    pretty (File start expr) = pretty start <> pretty expr

instance Pretty Token where
    pretty = text . tokenText

instance Pretty [Token] where
    pretty = hcat
