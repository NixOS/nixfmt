{-# LANGUAGE TypeOperators #-}

module Nixfmt.Parser.Float (floatParse) where

import Data.List (singleton)
import Data.Text (Text, pack)
import Text.Megaparsec (
  MonadParsec,
  Token,
  many,
  oneOf,
  option,
  satisfy,
  some,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (char, digitChar)

floatParse :: (MonadParsec e s m, Token s ~ Char, Semigroup (m [Char])) => m Text
floatParse =
  -- This mirrors https://github.com/NixOS/nix/blob/b89eca9aecc69d4dfc0f0afd9353c126eb7b5858/src/libexpr/lexer.l#L96:
  -- (([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?
  pack
    <$> ( ( try oneThroughNineStart
              <|> zeroDotStart
          )
            <> scientific
        )
  where
    oneThroughNineStart = oneThroughNine <> many digitChar <> (singleton <$> char '.') <> many digitChar
    zeroDotStart = option "" (singleton <$> char '0') <> (singleton <$> char '.') <> some digitChar
    scientific = option "" ((singleton <$> oneOf "Ee") <> option "" (singleton <$> oneOf "+-") <> some digitChar)
    oneThroughNine = singleton <$> satisfy (\c -> '1' <= c && c <= '9')
