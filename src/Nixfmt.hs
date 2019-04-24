module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , formatIO
    ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding (Pretty, width)
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Traversable
import System.IO
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (Pretty, pretty')
import Nixfmt.Pretty ()
import Nixfmt.Types (ParseErrorBundle)

layout :: Int -> Doc ann -> SimpleDocStream ann
layout w = layoutSmart (LayoutOptions{layoutPageWidth = AvailablePerLine w 1.0})

hPutDocW :: Pretty a => Handle -> Int -> a -> IO ()
hPutDocW output width = renderIO output . layout width . pretty'

formatIO :: Int -> String -> Text -> Handle -> IO (Either ParseErrorBundle ())
formatIO width filename input output =
    for (parse file filename input) (hPutDocW output width)
