{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent
import Control.Concurrent.ParallelIO.Local
import Data.Either
import qualified Data.Text.IO as TextIO
import GHC.Conc (numCapabilities)
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.IO
import System.Posix.Process (exitImmediately)
import System.Posix.Signals

import Nixfmt

type Result = Either ParseErrorBundle ()

data Options = Options
    { files :: [FilePath]
    , width :: Int
    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    { files = def &= args &= typ "FILES/DIRS"
    , width = def &= opt (80 :: Int) &= help "Maximum width of the formatted file"
    } &= help "Format Nix source code"

formatStdio :: Int -> IO Result
formatStdio w = do
    contents <- TextIO.getContents
    formatIO w "<stdin>" contents stdout

formatFile :: Int -> FilePath -> IO Result
formatFile w path = do
    contents <- TextIO.readFile path
    withFile path WriteMode $ formatIO w path contents

doParallel :: [IO a] -> IO [a]
doParallel = withPool numCapabilities . flip parallelInterleaved

findRecursive :: FilePath -> IO [FilePath]
findRecursive = listDirectory

errorWriter :: Chan (Maybe String) -> IO ()
errorWriter chan = do
    item <- readChan chan
    case item of
         Nothing  -> return ()
         Just msg -> hPutStr stderr msg >> errorWriter chan

writeErrorBundle :: Chan (Maybe String) -> Result -> IO Result
writeErrorBundle chan result = do
    case result of
         Right () -> return ()
         Left err -> writeChan chan $ Just $ errorBundlePretty err
    return result

formatParallel :: [IO Result] -> IO [Result]
formatParallel jobs = do
    errChan <- newChan
    _ <- forkIO $ errorWriter errChan
    results <- doParallel $ map (>>=(writeErrorBundle errChan)) jobs
    writeChan errChan Nothing
    return results

main :: IO ()
main = do
    _ <- installHandler keyboardSignal
            (Catch (exitImmediately $ ExitFailure 2)) Nothing
    opts <- cmdArgs options
    results <- formatParallel $ case files opts of
                   [] -> pure $ formatStdio (width opts)
                   _  -> map (formatFile (width opts)) (files opts)

    case lefts results of
         [] -> exitSuccess
         _  -> exitFailure
