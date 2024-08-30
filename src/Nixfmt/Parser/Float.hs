module Nixfmt.Parser.Float (floatParse) where

import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (foldl')
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific, toRealFloat)
import Text.Megaparsec (
  MonadParsec,
  Token,
  chunkToTokens,
  notFollowedBy,
  option,
  takeWhile1P,
  try,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, char', digitChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)

-- copied (and modified) from Text.Megaparsec.Char.Lexer
data SP = SP !Integer {-# UNPACK #-} !Int

floatParse :: (MonadParsec e s m, Token s ~ Char, RealFloat a) => m a
floatParse = do
  notFollowedBy $ char '0' >> digitChar
  notFollowedBy (char' 'e')
  c' <- (decimal <?> "decimal") <|> return 0
  toRealFloat
    <$> ( ( do
              SP c e' <- dotDecimal_ c'
              e <- option e' (try $ exponent_ e')
              return (scientific c e)
          )
            <|> (scientific c' <$> exponent_ 0)
        )
{-# INLINE floatParse #-}

-- copied from Text.Megaparsec.Char.Lexer
dotDecimal_ ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Char) =>
  Integer ->
  m SP
dotDecimal_ c' = do
  void (char '.')
  let mkNum = foldl' step (SP c' 0) . chunkToTokens @s Proxy
      step (SP a e') c =
        SP
          (a * 10 + fromIntegral (Char.digitToInt c))
          (e' - 1)
  mkNum <$> takeWhile1P (Just "digit") Char.isDigit
{-# INLINE dotDecimal_ #-}

-- copied from Text.Megaparsec.Char.Lexer
exponent_ :: (MonadParsec e s m, Token s ~ Char) => Int -> m Int
exponent_ e' = do
  void (char' 'e')
  (+ e') <$> signed (return ()) decimal
{-# INLINE exponent_ #-}
