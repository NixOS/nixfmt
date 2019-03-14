module Main where

import           Data.Text.IO    hiding (putStrLn)
import           Nixfmt.Parser
import           Prelude         hiding (getContents)
import           Text.Megaparsec

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ show $ parse nixFile "<stdin>" contents
