{-# LANGUAGE FlexibleInstances #-}
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
    pretty (TrailingComment tc) = text " #" <> pretty tc <> hardline
    pretty (LineComment lc)     = text "#" <> pretty lc <> hardline
    pretty (BlockComment bc)
        = text "/*" <>
          hcat (intersperse hardline $ map pretty bc) <>
          text "*/"

    prettyList []                                = emptyDoc
    prettyList (tc@(TrailingComment _) : trivia) = pretty tc <>
                                                   hcat (map pretty trivia)
    prettyList trivia                            = hardline <>
                                                   hcat (map pretty trivia)

instance Pretty NixToken where
    pretty (Identifier i) = text i
    pretty (Trivia t)     = pretty t

    pretty t              = pretty $ show t

split :: NixToken -> [NixAST] -> ([NixAST], NixAST, [NixAST])
split t xs =
    let (leading, matching : trailing) = span (/= Leaf t) xs
    in (leading, matching, trailing)

instance Pretty NixAST where
    pretty (Leaf l) = pretty l

    pretty (Node File children) = pretty children

    pretty (Node List children0) =
        let (leading, brackOpen, children1) = split TBrackOpen children0
            (items, brackClose, trailing) = split TBrackClose children1
        in pretty leading <>
           group (pretty brackOpen <>
                  nest 2 (case (filter (/=Leaf (Trivia [])) items) of
                      []        -> emptyDoc
                      someItems -> line <>
                                   vsep (map pretty someItems) <>
                                   line) <>
                  pretty brackClose) <>
           pretty trailing

    pretty node     = pretty $ show node

    prettyList nodes = hcat (map pretty nodes)
