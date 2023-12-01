{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , format
    , formatVerify
    ) where

import Data.Function ((&))
import Data.Bifunctor (bimap, first)
import Data.Text (Text, unpack)
import qualified Text.Megaparsec as Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (layout)
import Nixfmt.Pretty ()
import Nixfmt.Types (ParseErrorBundle)

type Width = Int

-- | @format w filename source@ returns either a parsing error specifying a
-- failure in @filename@ or a formatted version of @source@ with a maximum width
-- of @w@ columns where possible.
format :: Width -> FilePath -> Text -> Either String Text
format width filename
    = bimap errorBundlePretty (layout width)
    . Megaparsec.parse file filename

formatVerify :: Width -> FilePath -> Text -> Either String Text
formatVerify width path unformatted = do
    unformattedParsed <- parse unformatted
    let formattedOnce = layout width unformattedParsed
    formattedOnceParsed <- flip first (parse formattedOnce) $
        (\x -> pleaseReport "Fails to parse after formatting.\n" <> x <> "\n\nAfter Formatting:\n" <> unpack formattedOnce)
    let formattedTwice = layout width formattedOnceParsed
    if formattedOnceParsed /= unformattedParsed
    then Left $ pleaseReport "Parses differently after formatting." &
        \x -> (x <> "\n\nBefore formatting:\n" <> (show unformattedParsed) <> "\n\nAfter formatting:\n" <> (show formattedOnceParsed))
    else if formattedOnce /= formattedTwice
    then Left $ pleaseReport "Nixfmt is not idempotent." &
        \x -> (x <> "\n\nAfter one formatting:\n" <> unpack formattedOnce <> "\n\nAfter two:\n" <> unpack formattedTwice)
    else Right formattedOnce
    where
        parse = first errorBundlePretty . Megaparsec.parse file path
        pleaseReport x = path <> ": " <> x <> " This is a bug in nixfmt. Please report it at https://github.com/serokell/nixfmt"
