module Nixfmt.Util
    ( manyList
    , someList
    , manyP
    , someP
    , manyText
    , someText
    , commonPrefix
    , dropCommonIndentation
    ) where

import           Data.Char
import           Data.Maybe
import           Data.Text       as Text hiding (map, takeWhile)
import           Data.Void
import           Text.Megaparsec hiding (empty)

type Parser = Parsec Void Text

someP :: (Char -> Bool) -> Parser Text
someP = takeWhile1P Nothing

manyP :: (Char -> Bool) -> Parser Text
manyP = takeWhileP Nothing

someText :: Parser Text -> Parser Text
someText p = Text.concat <$> some p

manyText :: Parser Text -> Parser Text
manyText p = Text.concat <$> many p

someList :: Parser [a] -> Parser [a]
someList p = Prelude.concat <$> some p

manyList :: Parser [a] -> Parser [a]
manyList p = Prelude.concat <$> many p

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x : xs) (y : ys) | x == y = x : commonPrefix xs ys
commonPrefix _ _               = []

commonIndentation :: [String] -> String
commonIndentation []        = []
commonIndentation [x]       = takeWhile isSpace x
commonIndentation (x:"":xs) = commonIndentation (x:xs)
commonIndentation (x:y:xs)  = commonIndentation (commonPrefix x y : xs)

dropCommonIndentation :: [Text] -> [Text]
dropCommonIndentation unstrippedLines =
    let strippedLines = map stripEnd unstrippedLines
        indentation = pack $ commonIndentation $ map unpack strippedLines
    in map (fromMaybe empty . stripPrefix indentation) strippedLines
