module Main where

import           Data.Text.IO  as TextIO (getContents)
import           System.IO     (hPutStr, putStr, stderr)

import           Nixfmt.Parser

main :: IO ()
main = do
    contents <- TextIO.getContents
    case parse file "<stdin>" contents of
        Left err     -> hPutStr stderr $ errorBundlePretty err
        Right parsed -> putStr $ show parsed
