module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , Width
    , format
    , formatVerify
    ) where

import Data.Either (fromRight)
import Data.Bifunctor (bimap, first)
import Data.Text (Text, unpack)
import qualified Text.Megaparsec as Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (layout)
import Nixfmt.Pretty ()
import Nixfmt.Types (ParseErrorBundle, Whole(..), Expression, walkSubprograms)

-- import Debug.Trace (traceShow, traceShowId)

type Width = Int

-- | @format w filename source@ returns either a parsing error specifying a
-- failure in @filename@ or a formatted version of @source@ with a maximum width
-- of @w@ columns where possible.
format :: Width -> FilePath -> Text -> Either String Text
format width filename
    = bimap errorBundlePretty (layout width)
    . Megaparsec.parse file filename

-- Same functionality as `format`, but add sanity checks to guarantee the following properties of the formatter:
-- - Correctness: The formatted output parses, and the parse tree is identical to the input's
-- - Idempotency: Formatting the output again will not modify it
--
-- If any issues are found, the operation will fail and print an error message. It will contain a diff showcasing
-- the issue on an automatically minimized example based on the input.
formatVerify :: Width -> FilePath -> Text -> Either String Text
formatVerify width path unformatted = do
    unformattedParsed@(Whole unformattedParsed' _) <- parse unformatted
    let formattedOnce = layout width unformattedParsed
    formattedOnceParsed <- first (\x -> pleaseReport "Fails to parse after formatting.\n" <> x <> "\n\nAfter Formatting:\n" <> unpack formattedOnce) (parse formattedOnce)
    let formattedTwice = layout width formattedOnceParsed
    if formattedOnceParsed /= unformattedParsed
    then Left $
        let
            minimized = minimize unformattedParsed' (\e -> parse (layout width e) == Right (Whole e []))
        in
        pleaseReport "Parses differently after formatting."
        <> "\n\nBefore formatting:\n" <> show minimized
        <> "\n\nAfter formatting:\n" <> show (fromRight (error "TODO") $ parse (layout width minimized))
    else if formattedOnce /= formattedTwice
    then Left $
        let
            minimized = minimize unformattedParsed'
                (\e -> layout width e == layout width (fromRight (error "TODO") $ parse $ layout width e))
        in
        pleaseReport "Nixfmt is not idempotent."
        <> "\n\nAfter one formatting:\n" <> unpack (layout width minimized)
        <> "\n\nAfter two:\n" <> unpack (layout width (fromRight (error "TODO") $ parse $ layout width minimized))
    else Right formattedOnce
    where
        parse = first errorBundlePretty . Megaparsec.parse file path
        pleaseReport x = path <> ": " <> x <> " This is a bug in nixfmt. Please report it at https://github.com/NixOS/nixfmt"


minimize :: Expression -> (Expression -> Bool) -> Expression
minimize expr test =
    case concatMap (\e -> ([minimize e test | not (test e)])) $ walkSubprograms expr of
         result:_ -> result
         [] -> expr
