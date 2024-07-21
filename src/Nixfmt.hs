module Nixfmt (
  errorBundlePretty,
  ParseErrorBundle,
  Width,
  format,
  formatVerify,
  printAst,
)
where

import Data.Bifunctor (bimap, first)
import Data.Either (fromRight)
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import qualified Nixfmt.Parser as Parser
import Nixfmt.Predoc (LayoutMode, layout)
import Nixfmt.Pretty ()
import Nixfmt.Types (Expression, ParseErrorBundle, Whole (..), walkSubprograms)
import qualified Text.Megaparsec as Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple (pShow)

-- import Debug.Trace (traceShow, traceShowId)

type Width = Int

-- | @format w filename source@ returns either a parsing error specifying a
-- failure in @filename@ or a formatted version of @source@ with a maximum width
-- of @w@ columns where possible.
format :: LayoutMode -> Width -> FilePath -> Text -> Either String Text
format mode width filename =
  bimap errorBundlePretty (layout mode width)
    . Megaparsec.parse Parser.file filename

-- | Pretty print the internal AST for debugging
printAst :: FilePath -> Text -> Either String Text
printAst path unformatted = do
  Whole unformattedParsed' _ <- first errorBundlePretty . Megaparsec.parse Parser.file path $ unformatted
  Left (unpack $ toStrict $ pShow unformattedParsed')

-- Same functionality as `format`, but add sanity checks to guarantee the following properties of the formatter:
-- - Correctness: The formatted output parses, and the parse tree is identical to the input's
-- - Idempotency: Formatting the output again will not modify it
--
-- If any issues are found, the operation will fail and print an error message. It will contain a diff showcasing
-- the issue on an automatically minimized example based on the input.
formatVerify :: LayoutMode -> Width -> FilePath -> Text -> Either String Text
formatVerify mode width path unformatted = do
  unformattedParsed@(Whole unformattedParsed' _) <- parse unformatted
  let formattedOnce = layout mode width unformattedParsed
  formattedOnceParsed <- first (\x -> pleaseReport "Fails to parse after formatting.\n" <> x <> "\n\nAfter Formatting:\n" <> unpack formattedOnce) (parse formattedOnce)
  let formattedTwice = layout mode width formattedOnceParsed
  if formattedOnceParsed /= unformattedParsed
    then
      Left $
        let minimized = minimize unformattedParsed' (\e -> parse (layout mode width e) == Right (Whole e []))
        in pleaseReport "Parses differently after formatting."
            <> "\n\nBefore formatting:\n"
            <> show minimized
            <> "\n\nAfter formatting:\n"
            <> show (fromRight (error "TODO") $ parse (layout mode width minimized))
    else
      if formattedOnce /= formattedTwice
        then
          Left $
            let minimized =
                  minimize
                    unformattedParsed'
                    (\e -> layout mode width e == layout mode width (fromRight (error "TODO") $ parse $ layout mode width e))
            in pleaseReport "Nixfmt is not idempotent."
                <> "\n\nAfter one formatting:\n"
                <> unpack (layout mode width minimized)
                <> "\n\nAfter two:\n"
                <> unpack (layout mode width (fromRight (error "TODO") $ parse $ layout mode width minimized))
        else Right formattedOnce
  where
    parse = first errorBundlePretty . Megaparsec.parse Parser.file path
    pleaseReport x = path <> ": " <> x <> " This is a bug in nixfmt. Please report it at https://github.com/NixOS/nixfmt"

minimize :: Expression -> (Expression -> Bool) -> Expression
minimize expr test =
  case concatMap (\e -> ([minimize e test | not (test e)])) $ walkSubprograms expr of
    result : _ -> result
    [] -> expr
