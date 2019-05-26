{- © 2019 Serokell <hi@serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Helpers for implementing tools that process UTF-8 encoded data.
--
-- Standard IO functions assume that the character encoding of the data
-- they read or write is the same as the one used by current locale. In many
-- situtations this assumption is wrong, as tools work with files, and
-- the files nowadays are mostly UTF-8 encoded, regardless of the locale.
-- Therefore, it is almost always a good idea to switch the encoding of
-- file handles to UTF-8.
--
-- The same applies to standard input, output, and error handles. However,
-- there is an edge-case: if they are attached to a terminal, and the
-- encoding is not UTF-8, using UTF-8 might actually be unsafe.
--
-- Functions in this module help deal with all these issues.
module System.IO.Utf8
  ( withUtf8StdHandles

  , openFileUtf8
  , readFileUtf8
  ) where

import Control.Exception (bracket)
import Data.Text (Text)
import GHC.IO.Encoding (mkTextEncoding, textEncodingName, utf8)
import System.IO (stderr, stdin, stdout)

import qualified Data.Text.IO as T
import qualified System.IO as IO


type EncRestoreAction = IO.Handle -> IO ()

-- | Sets the best available UTF-8-compatible encoding for the handle.
-- Returns the action that will restore the previous one.
--
-- If the handle is in binary mode, does nothing.
-- If the handle is not attached to a terminal, sets UTF-8.
-- Otherwise, keeps its current encoding, but augments it to transliterate
-- unsupported characters.
hSetBestUtf8Enc :: IO.Handle -> IO EncRestoreAction
hSetBestUtf8Enc h = IO.hGetEncoding h >>= \case
    Nothing -> pure (\_ -> pure ())
    Just enc -> do
      isTerm <- IO.hIsTerminalDevice h
      enc' <- chooseBestEnc isTerm enc
      IO.hSetEncoding h enc'
      pure $ flip IO.hSetEncoding enc
  where
    chooseBestEnc False _ = pure utf8
    chooseBestEnc True enc@(textEncodingName -> name)
      | '/' `notElem` name = mkTextEncoding (name ++ "//TRANSLIT")
      | otherwise = pure enc

-- | Configures the encodings of the three standard handles (stdin, stdout, stderr)
-- to work with UTF-8 encoded data and runs the specified IO action.
-- After the action finishes, restores the original encodings.
withUtf8StdHandles :: IO a -> IO a
withUtf8StdHandles action =
    withConfiguredHandle stdin $
    withConfiguredHandle stdout $
    withConfiguredHandle stderr $
      action
  where
    withConfiguredHandle :: IO.Handle -> IO a -> IO a
    withConfiguredHandle h = bracket (hSetBestUtf8Enc h) ($ h) . const



-- | Like @openFile@, but sets the file encoding to UTF-8, regardless
-- of the current locale.
openFileUtf8 :: IO.FilePath -> IO.IOMode -> IO IO.Handle
openFileUtf8 path mode = do
  h <- IO.openFile path mode
  IO.hSetEncoding h utf8
  pure h

-- | Like @readFile@, but assumes the file is encoded in UTF-8, regardless
-- of the current locale.
readFileUtf8 :: IO.FilePath -> IO Text
readFileUtf8 path = openFileUtf8 path IO.ReadMode >>= T.hGetContents
