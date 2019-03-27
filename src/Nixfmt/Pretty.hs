{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Pretty where

import           Data.List                 hiding (group)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc hiding (prettyList)
import qualified Data.Text.Prettyprint.Doc
import           Nixfmt.Types

-- | This fixes ambiguity with OverloadedStrings when pretty is applied to a
-- String literal
text :: Text -> Doc ann
text = pretty

instance Pretty Trivium where
    pretty EmptyLine            = hardline
    pretty (LineComment lc)     = text "#" <> pretty lc <> hardline
    pretty (BlockComment bc)
        = text "/*" <>
          hcat (intersperse hardline $ map pretty bc) <>
          text "*/" <> hardline

    prettyList []                                = emptyDoc
    prettyList trivia                            = hardline <>
                                                   hcat (map pretty trivia)

instance Pretty NixToken where
    pretty (Identifier i) = text i

    pretty t              = pretty $ show t

split :: NixToken -> [NixAST] -> ([NixAST], NixAST, [NixAST])
split t xs =
    let matchLeaf (Leaf t' _) = t == t'
        matchLeaf _           = False
        (leading, matching : trailing) = span matchLeaf xs
    in (leading, matching, trailing)

thenLine :: NixAST -> Doc ann
thenLine (Leaf t Nothing) = pretty t <> line
thenLine x                = pretty x

instance Pretty NixAST where
    pretty (Leaf l Nothing) = pretty l
    pretty (Leaf l (Just comment)) = pretty l <> text (" #" <> comment) <> hardline
    pretty (Trivia t) = pretty t

    pretty (Node File children) = pretty children

    pretty (Node List children0) =
        let (leading, brackOpen, children1) = split TBrackOpen children0
            (items, brackClose, trailing) = split TBrackClose children1
        in pretty leading <>
           group (thenLine brackOpen <>
                  nest 2 (case (filter (/=Trivia []) items) of
                      []        -> emptyDoc
                      someItems -> hcat (map thenLine someItems)) <>
                  pretty brackClose) <>
           pretty trailing

    pretty node     = pretty $ show node

    prettyList nodes = hcat (map pretty nodes)
