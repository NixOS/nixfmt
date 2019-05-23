{- © 2019 Serokell <hi@serokell.io>
 - © 2019Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Data.Either (lefts)
import qualified Data.Text.IO as TextIO (getContents, putStr, readFile)
import System.AtomicWrite.Writer.Text (atomicWriteFile)
import System.Console.CmdArgs (Data, Typeable, args, cmdArgs, help, typ, (&=))
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.IO (hPutStr, stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler(..), installHandler, keyboardSignal)

import Nixfmt

type Result = Either ParseErrorBundle ()

data Nixfmt = Nixfmt
    { files :: [FilePath]
    , width :: Int
    } deriving (Show, Data, Typeable)

options :: Nixfmt
options = Nixfmt
    { files = [] &= args &= typ "FILES"
    , width = 80 &= help "Maximum width of the formatted file"
    } &= help "Format Nix source code"

formatStdio :: Int -> IO Result
formatStdio w = do
    contents <- TextIO.getContents
    mapM TextIO.putStr $ format w "<stdin>" contents

formatFile :: Int -> FilePath -> IO Result
formatFile w path = do
    contents <- TextIO.readFile path
    mapM (atomicWriteFile path) $ format w path contents

-- TODO: Efficient parallel implementation. This is just a sequential stub.
-- This was originally implemented using parallel-io, but it gave a factor two
-- overhead.
doParallel :: [IO a] -> IO [a]
doParallel = sequence

errorWriter :: Chan (Maybe String) -> Chan () -> IO ()
errorWriter chan done = do
    item <- readChan chan
    case item of
         Nothing  -> return ()
         Just msg -> hPutStr stderr msg >> errorWriter chan done
    writeChan done ()

writeErrorBundle :: Chan (Maybe String) -> Result -> IO Result
writeErrorBundle chan result = do
    case result of
         Right () -> return ()
         Left err -> writeChan chan $ Just $ errorBundlePretty err
    return result

formatParallel :: [IO Result] -> IO [Result]
formatParallel jobs = do
    errChan <- newChan
    doneChan <- newChan
    _ <- forkIO $ errorWriter errChan doneChan
    results <- doParallel $ map (>>= writeErrorBundle errChan) jobs
    writeChan errChan Nothing
    _ <- readChan doneChan
    return results

main :: IO ()
main = do
    _ <- installHandler keyboardSignal
            (Catch (exitImmediately $ ExitFailure 2)) Nothing
    opts <- cmdArgs options
    results <- formatParallel $ case files opts of
                   [] -> [formatStdio (width opts)]
                   _  -> map (formatFile (width opts)) (files opts)

    case lefts results of
         [] -> exitSuccess
         _  -> exitFailure
