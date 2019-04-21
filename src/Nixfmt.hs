module Nixfmt
    ( errorBundlePretty
    , file
    , parse
    , pretty
    , putDocW
    ) where

import           Text.Megaparsec
import           Text.Megaparsec.Error (errorBundlePretty)

import           Nixfmt.Parser         (file)
import           Nixfmt.Predoc         (pretty, putDocW)
import           Nixfmt.Pretty         ()
