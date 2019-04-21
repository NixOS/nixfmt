{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Pretty where

import           Prelude       hiding (break)

import           Data.List     hiding (break, group)
import           Data.Text     (pack)

import           Nixfmt.Predoc
import           Nixfmt.Types

instance Pretty Trivium where
    pretty EmptyLine            = emptyline
    pretty (LineComment lc)     = text "#" <> pretty lc <> hardline
    pretty (BlockComment bc)
        = text "/*" <>
          hcat (intersperse hardline $ map text bc) <>
          text "*/" <> hardline

instance Pretty [Trivium] where
    pretty [] = mempty
    pretty ts = hardline <> hcat (map pretty ts)

instance Pretty Token where
    pretty (Identifier i) = text i
    pretty (EnvPath p)    = text ("<" <> p <> ">")
    pretty (Path p)       = text p
    pretty (Integer i)    = text $ pack $ show i

    pretty t              = pretty $ show t

instance Pretty [Token] where
    pretty = hcat . map pretty

thenLine :: Pretty a => a -> Doc
thenLine node = pretty node <> space

hardspace :: Doc
hardspace = text " "

--instance Pretty Binder where
--    pretty (Assignment selectors assign expr semicolon) =
--        pretty selectors <> hardspace <> pretty assign <> softline <>
--        pretty expr <> pretty semicolon
--
--instance Pretty Term where
--    pretty (List paropen items parclose) =
--        group (thenLine paropen <>
--               nest 2 (hcat (map thenLine items)) <>
--               pretty parclose)
--
--    pretty (Set rec paropen bindings parclose) =
--        pretty (toList rec) <>
--        group (thenLine paropen <>
--               nest 2 (hcat (map thenLine bindings)) <>
--               thenLine parclose)
