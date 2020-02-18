{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , format
    ) where

import Data.Text (Text)
import Data.Semigroup (appEndo)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (layout)
import Nixfmt.Pretty ()
import Nixfmt.Transform (sortKeys)
import Nixfmt.Types (ParseErrorBundle)

-- | @format w filename source@ returns either a parsing error specifying a
-- failure in @filename@ or a formatted version of @source@ with a maximum width
-- of @w@ columns where possible.
format :: Int -> FilePath -> Text -> Either ParseErrorBundle Text
format width filename = fmap (layout width . transform) . parse file filename
  where
    transform = appEndo sortKeys
