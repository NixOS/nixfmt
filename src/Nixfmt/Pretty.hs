{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Pretty where

import           Data.List     hiding (group)
import           Data.Text     (Text, pack)
import qualified Data.Text     as Text
import           Nixfmt.Predoc
import           Nixfmt.Types

instance Pretty Trivium where
    pretty EmptyLine            = hardline
    pretty (LineComment lc)     = text "#" <> pretty lc <> hardline
    pretty (BlockComment bc)
        = text "/*" <>
          hcat (intersperse hardline $ map pretty bc) <>
          text "*/" <> hardline

instance Pretty [Trivium] where
    pretty []     = mempty
    pretty trivia = hardline <> hcat (map pretty trivia)

instance Pretty NixToken where
    pretty (Identifier i) = text i
    pretty (EnvPath p)    = text ("<" <> p <> ">")
    pretty (NixURI u)     = text u
    pretty (NixFloat f)   = text f
    pretty (NixInt i)     = text $ pack $ show i

    pretty t              = pretty $ show t

split :: NixToken -> [NixAST] -> ([NixAST], NixAST, [NixAST])
split t xs =
    let nonmatching (Leaf t' _) = t /= t'
        nonmatching _           = True
        (leading, matching : trailing) = span nonmatching xs
    in (leading, matching, trailing)

thenLine :: NixAST -> Doc
thenLine (Leaf t Nothing) = pretty t <> space
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
                  nest 2 (hcat (map thenLine items)) <>
                  thenLine brackClose) <>
           pretty trailing

    pretty node     = pretty $ show node

instance Pretty [NixAST] where
    pretty nodes = hcat (map pretty nodes)
