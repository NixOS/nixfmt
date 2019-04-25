module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , format
    , renderIO
    ) where

import Data.Text as Text (Text, pack, replicate)
import Data.Text.IO
import Data.Text.Prettyprint.Doc hiding (Pretty, width)
import System.IO (Handle, IO, hPutChar)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (pretty')
import Nixfmt.Pretty ()
import Nixfmt.Types (ParseErrorBundle)

-- | Render a SimpleDocStream to a file Handle, without indenting empty lines.
renderIO :: Handle -> SimpleDocStream ann -> IO ()
renderIO h = go
    where go SFail                      = error "unexpected SFail"
          go SEmpty                     = pure ()
          go (SChar c rest)             = hPutChar h c    >> go rest
          go (SText _ t rest)           = hPutStr h t     >> go rest
          go (SAnnPush _ rest)          =                    go rest
          go (SAnnPop rest)             =                    go rest
          go (SLine _ rest@(SLine _ _)) = hPutChar h '\n' >> go rest
          go (SLine n rest)             = do
              hPutChar h '\n'
              hPutStr h (Text.replicate n $ pack " ")
              go rest

layout :: Int -> Doc ann -> SimpleDocStream ann
layout w = layoutSmart (LayoutOptions{layoutPageWidth = AvailablePerLine w 1.0})

format :: Int -> FilePath -> Text -> Either ParseErrorBundle (SimpleDocStream ann)
format width filename = fmap (layout width . pretty') . parse file filename
