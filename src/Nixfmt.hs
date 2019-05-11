-- Copyright Serokell OU <hi@serokell.io>
-- Copyright Lars Jellema <lars.jellema@gmail.com>
-- SPDX-License-Identifier: MPL-2.0

module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , format
    ) where

import Data.Text (Text)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (layout)
import Nixfmt.Pretty ()
import Nixfmt.Types (ParseErrorBundle)

format :: Int -> FilePath -> Text -> Either ParseErrorBundle Text
format width filename = fmap (layout width) . parse file filename
