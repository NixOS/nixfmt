{-# LANGUAGE TupleSections #-}

module Nixfmt.Util
    ( manyP
    , someP
    , manyText
    , someText
    , commonIndentation
    , identChar
    , isSpaces
    , pathChar
    , schemeChar
    , uriChar
    ) where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text as Text
  (Text, all, commonPrefixes, concat, empty, takeWhile)
import Text.Megaparsec
  (MonadParsec, Token, Tokens, many, some, takeWhile1P, takeWhileP)

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
someP :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
someP = takeWhile1P Nothing

-- | Match zero or more characters that match a predicate.
manyP :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
manyP = takeWhileP Nothing

-- | Match one or more texts and return the concatenation.
someText :: MonadParsec e s m => m Text -> m Text
someText p = Text.concat <$> some p

-- | Match zero or more texts and return the concatenation.
manyText :: MonadParsec e s m => m Text -> m Text
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

isSpaces :: Text -> Bool
isSpaces = Text.all (==' ')
