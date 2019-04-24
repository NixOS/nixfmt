module Nixfmt
    ( errorBundlePretty
    , ParseErrorBundle
    , formatIO
    ) where

import Data.Text as Text (Text, pack, replicate)
import Data.Text.IO
import Data.Text.Prettyprint.Doc hiding (Pretty, width)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Traversable
import System.IO (Handle, IO, hPutChar)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

import Nixfmt.Parser (file)
import Nixfmt.Predoc (Pretty, pretty')
import Nixfmt.Pretty ()
import Nixfmt.Types (ParseErrorBundle)

renderIO :: Handle -> PP.SimpleDocStream ann -> IO ()
renderIO h = go
    where go SFail                      = return ()
          go SEmpty                     = return ()
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

hPutDocW :: Pretty a => Handle -> Int -> a -> IO ()
hPutDocW output width = renderIO output . layout width . pretty'

formatIO :: Int -> String -> Text -> Handle -> IO (Either ParseErrorBundle ())
formatIO width filename input output =
    for (parse file filename input) (hPutDocW output width)
