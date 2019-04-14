{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Pretty where

import           Data.List     hiding (break, group)
import           Data.Text     (Text, pack)
import qualified Data.Text     as Text
import           Nixfmt.Predoc
import           Nixfmt.Types
import           Prelude       hiding (break)

instance Pretty Trivium where
    pretty EmptyLine            = emptyline
    pretty (LineComment lc)     = text "#" <> pretty lc <> hardline
    pretty (BlockComment bc)
        = text "/*" <>
          hcat (intersperse hardline $ map text bc) <>
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
    pretty (NixText t)    = text t

    pretty t              = pretty $ show t

split :: (NixAST -> Bool) -> [NixAST] -> Maybe ([NixAST], NixAST, [NixAST])
split pred xs = case span (not . pred) xs of
    (leading, matching : trailing) -> Just (leading, matching, trailing)
    _                              -> Nothing

splitT :: NixToken -> [NixAST] -> Maybe ([NixAST], NixAST, [NixAST])
splitT t = split $ \case (Leaf t' _) -> t == t'
                         _           -> False

splitN :: NodeType -> [NixAST] -> Maybe ([NixAST], NixAST, [NixAST])
splitN n = split $ \case (Node n' _) -> n == n'
                         _           -> False

thenLine :: NixAST -> Doc
thenLine node = pretty node <> space

instance Pretty NixAST where
    pretty (Leaf l Nothing) = pretty l
    pretty (Leaf l (Just comment)) = pretty l <> text (" #" <> comment) <> hardline
    pretty (Trivia t) = pretty t

    pretty (Node File children) = pretty children <> hardline

    pretty (Node List children0) =
        let Just (leading, brackOpen, children1) = splitT TBrackOpen children0
            Just (items, brackClose, trailing) = splitT TBrackClose children1
        in pretty leading <>
           group (thenLine brackOpen <>
                  nest 2 (hcat (map thenLine items)) <>
                  pretty brackClose) <>
           pretty trailing

    pretty (Node Set children0) =
        let Just (leading, braceOpen, children1) = splitT TBraceOpen children0
            Just (bindings, braceClose, trailing) = splitT TBraceClose children1
        in pretty leading <>
           group (thenLine braceOpen <>
                  nest 2 (hcat (map thenLine bindings)) <>
                  thenLine braceClose) <>
           pretty trailing

    pretty (Node Assignment children0) =
        let Just (leading, assignToken, children1) = splitT TAssign children0
            Just (expression, semicolon, trailing) = splitT TSemicolon children1
        in pretty leading <> text " " <> pretty assignToken <> softline <>
           pretty expression <> pretty semicolon <> pretty trailing

    pretty (Node With children0) =
        let Just (leading, withToken, children1) = splitT TWith children0
            Just (expr1, semicolon, expr2) = splitT TSemicolon children1
        in pretty leading <> pretty withToken <> text " " <>
           pretty expr1 <> pretty semicolon <> softline <> pretty expr2

    pretty (Node Let children0) =
        let Just (leading, letToken, children1) = splitT TLet children0
            Just (bindings, inToken, expression) = splitT TIn children1
        in pretty leading <> space <> thenLine letToken <>
           nest 2 (hcat (map (group . thenLine) bindings)) <>
           emptyline <> pretty inToken <> text " " <> pretty expression

    pretty (Node Abstraction children0) =
        let Just (parameter, colon, expression) = splitT TColon children0
        in group $ pretty parameter <> pretty colon <> space <> pretty expression

    pretty (Node ContextParameter children0) = hcat $ map pretty children0

    pretty (Node SetParameter children0) =
        let Just (leading, braceOpen, children1) = splitT TBraceOpen children0
            Just (params, braceClose, trailing) = splitT TBraceClose children1
            go xs = case splitT TComma xs of
                         Just (before, comma, after)
                                 -> nest 2 (pretty before) <> break <>
                                    pretty comma <> text " " <>
                                    go after
                         Nothing -> pretty xs
        in pretty leading <>
           group (pretty braceOpen <> softline <> go params <>
                  softline <> pretty braceClose) <> pretty trailing

    pretty (Node AttrParameter children0) =
        case splitT TQuestion children0 of
             Just (name, question, expr)
                     -> group $ pretty name <> softline <>
                        pretty question <> space <>
                        nest 2 (pretty expr)
             Nothing -> pretty children0

    pretty (Node Inherit children0) =
        let Just (leading, keyword, children1) = splitT TInherit children0
            Just (items, semicolon, trailing) = splitT TSemicolon children1
        in pretty leading <> pretty keyword <>
           hcat (map (\x -> softline <> pretty x) items) <>
           pretty semicolon <> pretty trailing

    pretty (Node Parenthesized children) = pretty children
    pretty (Node SimpleString children) = pretty children
    pretty (Node IndentedString children) = pretty children
    pretty (Node Interpolation children) = pretty children

    pretty (Node Application (first : xs)) = pretty first <>
        hcat (map (\x -> softline <> pretty x) xs)

    pretty node     = pretty $ show node

instance Pretty [NixAST] where
    pretty nodes = hcat (map pretty nodes)
