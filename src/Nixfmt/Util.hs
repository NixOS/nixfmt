module Nixfmt.Util
    ( manyP
    , someP
    , manyText
    , someText
    , commonPrefix
    , commonIndentation
    , dropCommonIndentation
    ) where

import Data.Char
import Data.Maybe
import Data.Text as Text hiding (filter, map)
import Text.Megaparsec hiding (empty)

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

-- | The longest common prefix consisting of only whitespace.
commonIndentation :: [Text] -> Text
commonIndentation []       = empty
commonIndentation [x]      = Text.takeWhile isSpace x
commonIndentation (x:y:xs) = commonIndentation (commonPrefix x y : xs)

-- | Strip the longest common indentation from a list of lines. Empty lines do
-- not count towards the common indentation.
dropCommonIndentation :: [Text] -> [Text]
dropCommonIndentation unstrippedLines =
    let strippedLines = map stripEnd unstrippedLines
        indentation = commonIndentation $ filter (/=empty) strippedLines
    in map (fromMaybe empty . stripPrefix indentation) strippedLines
