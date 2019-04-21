module Main where

import           Data.Text.IO    (getContents)
import           Nixfmt.Parser
import           Prelude         hiding (getContents)
import           System.IO       (hPutStr, putStr, stderr)
import           Text.Megaparsec

main :: IO ()
main = do
    contents <- getContents
    case parse file "<stdin>" contents of
        Left err     -> hPutStr stderr $ errorBundlePretty err
        Right parsed -> putStr $ show parsed
