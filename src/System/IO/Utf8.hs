{- © 2019 Serokell <hi@serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ViewPatterns       #-}

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
  ( withUtf8Stds

  , readFileUtf8
  ) where

import Control.Exception (bracket)
import Data.Foldable (sequenceA_)
import Data.Text (Text)
import GHC.IO.Encoding (mkTextEncoding, textEncodingName, utf8)
import System.IO (stderr, stdin, stdout)

import qualified Data.Text.IO as T
import qualified System.IO as IO


-- | A tripple of data for the three std* handles.
data StdHandlesData a = StdHandlesData
  { shdStdin :: a
  , shdStdout :: a
  , shdStderr :: a
  } deriving (Eq, Foldable, Functor, Show, Traversable)

instance Applicative StdHandlesData where
  pure a = StdHandlesData a a a
  StdHandlesData fi fo fe <*> StdHandlesData ai ao ae =
    StdHandlesData (fi ai) (fo ao) (fe ae)

stdHandles :: StdHandlesData IO.Handle
stdHandles = StdHandlesData stdin stdout stderr


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

-- | Configures the encodings of the three standard handles to work kwith UTF-8
-- and runs the specified IO action. After the action finishes, restores the
-- encodings.
withUtf8Stds :: IO a -> IO a
withUtf8Stds action = bracket setUtf8Encs resetEncs (const action)
  where
    -- | For each of the three std handles, sets the best UTF-8-compatible
    -- encoding and returns actions that undo the change.
    setUtf8Encs :: IO (StdHandlesData EncRestoreAction)
    setUtf8Encs = sequenceA (hSetBestUtf8Enc <$> stdHandles)

    -- | Takes a triple of encodings and sets them on the three std handles.
    resetEncs :: StdHandlesData EncRestoreAction -> IO ()
    resetEncs actions = sequenceA_ (actions <*> stdHandles)



-- | Like @readFile@, but assumes the file is encoded in UTF-8, regardless
-- of the current locale.
readFileUtf8 :: IO.FilePath -> IO Text
readFileUtf8 path = do
  h <- IO.openFile path IO.ReadMode
  IO.hSetEncoding h utf8
  T.hGetContents h
