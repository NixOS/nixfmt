{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Nixfmt.Util
    ( manyP
    , someP
    , manyText
    , someText
    , commonPrefix
    , commonIndentation
    , dropCommonIndentation
    , identChar
    , pathChar
    , schemeChar
    , uriChar
    ) where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text as Text
  (Text, commonPrefixes, concat, empty, stripEnd, stripPrefix, takeWhile)
import Text.Megaparsec
  (ParsecT, Stream, Token, Tokens, many, some, takeWhile1P, takeWhileP)

charClass :: [Char] -> Char -> Bool
charClass s c = isAlpha c || isDigit c || elem c s

identChar :: Char -> Bool
identChar = charClass "_'-"

pathChar :: Char -> Bool
pathChar = charClass "._-+~"

schemeChar :: Char -> Bool
schemeChar = charClass "-.+"

uriChar :: Char -> Bool
uriChar = charClass "~!@$%&*-=_+:',./?"

-- | Match one or more characters that match a predicate.
someP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
someP = takeWhile1P Nothing

-- | Match zero or more characters that match a predicate.
manyP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
manyP = takeWhileP Nothing

-- | Match one or more texts and return the concatenation.
someText :: (Stream s, Ord e) => ParsecT e s m Text -> ParsecT e s m Text
someText p = Text.concat <$> some p

-- | Match zero or more texts and return the concatenation.
manyText :: (Stream s, Ord e) => ParsecT e s m Text -> ParsecT e s m Text
manyText p = Text.concat <$> many p

-- | The longest common prefix of the arguments.
commonPrefix :: Text -> Text -> Text
commonPrefix a b =
    case commonPrefixes a b of
         Nothing             -> empty
         Just (prefix, _, _) -> prefix

-- | The longest common prefix consisting of only whitespace. The longest common
-- prefix of zero texts is infinite, represented as Nothing.
commonIndentation :: [Text] -> Maybe Text
commonIndentation []       = Nothing
commonIndentation [x]      = Just $ Text.takeWhile isSpace x
commonIndentation (x:y:xs) = commonIndentation (commonPrefix x y : xs)

-- | Strip the longest common indentation from a list of lines. Empty lines do
-- not count towards the common indentation.
dropCommonIndentation :: [Text] -> [Text]
dropCommonIndentation unstrippedLines =
    let strippedLines = map stripEnd unstrippedLines
    in case commonIndentation (filter (/=empty) strippedLines) of
            Nothing          -> map (const empty) strippedLines
            Just indentation -> map (fromMaybe empty . stripPrefix indentation) strippedLines
