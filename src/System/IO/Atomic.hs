{- © 2019 Serokell <hi@serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Helpers for atomic file IO.
--
-- This module is a crossover of the `atomic-write` and `safeio` libraries
-- with the fs-related behaviour of the first and interface of the second.
module System.IO.Atomic
  ( withOutputFile
  ) where

import Control.Exception (bracketOnError)
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, openTempFileWithDefaultPermissions)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)


withOutputFile ::
     FilePath  -- ^ Final file path
  -> (Handle -> IO a)  -- ^ IO action that writes to the file
  -> IO a
withOutputFile path act = bracketOnError begin rollback $ \(tpath, th) -> do
    copyAttributes (tpath, th)
    result <- act th
    commit (tpath, th)
    pure result
  where
    tmpDir = takeDirectory path
    tmpTemplate = "." <> takeFileName path <> ".atomic"

    begin :: IO (FilePath, Handle)
    begin = openTempFileWithDefaultPermissions tmpDir tmpTemplate

    -- This function is Unix-specific...
    copyAttributes :: (FilePath, Handle) -> IO ()
    copyAttributes (tpath, _th) = do
      exists <- doesFileExist path
      when exists $ do
        getFileStatus path >>= setFileMode tpath . fileMode

    commit :: (FilePath, Handle) -> IO ()
    commit (tpath, th) = hClose th *> renameFile tpath path

    rollback :: (FilePath, Handle) -> IO ()
    rollback (tpath, th) = hClose th *> removeFile tpath
