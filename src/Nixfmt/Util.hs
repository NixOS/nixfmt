module Nixfmt.Util
    ( commonPrefix
    , dropCommonIndentation
    ) where

import           Data.Char
import           Data.Maybe
import           Data.Text  hiding (map, takeWhile)

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x : xs) (y : ys) | x == y = x : commonPrefix xs ys
commonPrefix _ _               = []

commonIndentation :: [String] -> String
commonIndentation []        = []
commonIndentation [x]       = takeWhile isSpace x
commonIndentation (x:"":xs) = commonIndentation (x:xs)
commonIndentation (x:y:xs)  = commonIndentation (commonPrefix x y : xs)

dropCommonIndentation :: [Text] -> [Text]
dropCommonIndentation lines =
    let strippedLines = map stripEnd lines
        indentation = pack $ commonIndentation $ map unpack strippedLines
    in map (fromMaybe empty . stripPrefix indentation) strippedLines
