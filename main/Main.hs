module Main where

import           Data.Text.IO as TextIO (getContents)
import           System.Exit  (die)

import           Nixfmt

main :: IO ()
main = do
    contents <- TextIO.getContents
    case parse file "<stdin>" contents of
        Left err     -> die $ errorBundlePretty err
        Right parsed -> putDocW 80 parsed
