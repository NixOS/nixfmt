{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , Width
    , format
    , formatVerify
    ) where

import Data.Bifunctor (bimap, first)
import Data.Text (Text)
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
    formattedOnceParsed <- parse formattedOnce
    let formattedTwice = layout width formattedOnceParsed
    if formattedOnceParsed /= unformattedParsed
    then pleaseReport "Parses differently after formatting."
    else if formattedOnce /= formattedTwice
    then pleaseReport "Nixfmt is not idempotent."
    else Right formattedOnce
    where
        parse = first errorBundlePretty . Megaparsec.parse file path
        pleaseReport x = Left $ path <> ": " <> x <> " This is a bug in nixfmt. Please report it at https://github.com/serokell/nixfmt"
