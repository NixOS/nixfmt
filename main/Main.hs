{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}

module Main where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Either (lefts)
import Data.Text (Text)
import Data.Version (showVersion)
import GHC.IO.Encoding (utf8)
import Paths_nixfmt (version)
import System.Console.CmdArgs
  (Data, Typeable, args, cmdArgs, help, summary, typ, (&=))
import System.Directory (pathIsSymbolicLink)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.IO (hPutStrLn, hSetEncoding, stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler(..), installHandler, keyboardSignal)

import qualified Data.Text.IO as TextIO (getContents, hPutStr, putStr)

import Nixfmt
import System.IO.Atomic (withOutputFile)
import System.IO.Utf8 (readFileUtf8, withUtf8StdHandles)

type Result = Either String ()
type Width = Int

data Nixfmt = Nixfmt
    { files :: [FilePath]
    , width :: Width
    , check :: Bool
    , quiet :: Bool
    } deriving (Show, Data, Typeable)

options :: Nixfmt
options = Nixfmt
    { files = [] &= args &= typ "FILES"
    , width = 80 &= help "Maximum width in characters"
    , check = False &= help "Check whether files are formatted"
    , quiet = False &= help "Do not report errors"
    } &= summary ("nixfmt v" ++ showVersion version)
    &= help "Format Nix source code"

format' :: Width -> FilePath -> Text -> Either String Text
format' w path = first errorBundlePretty . format w path

data Target
    = StdioTarget
    | FileTarget FilePath

targetName :: Target -> String
targetName StdioTarget = "<stdin>"
targetName (FileTarget path) = path

readTarget :: Target -> IO Text
readTarget StdioTarget = TextIO.getContents
readTarget (FileTarget path) = readFileUtf8 path

filterTarget :: (Target -> IO Result) -> Target -> IO Result
filterTarget operation StdioTarget = operation StdioTarget
filterTarget operation (FileTarget path) = do
    isSymlink <- try $ pathIsSymbolicLink path
    case isSymlink of
         Right True  -> return $ Left $ path ++ ": ignoring symlink"
         Right False -> operation (FileTarget path)
         Left e      -> return $ Left $ show (e :: IOException)

writeTarget :: Target -> Text -> IO ()
writeTarget StdioTarget t = TextIO.putStr t
writeTarget (FileTarget path) t = withOutputFile path $ \h -> do
    hSetEncoding h utf8
    TextIO.hPutStr h t

formatTarget :: Width -> Target -> IO Result
formatTarget w target = do
    contents <- readTarget target
    let formatted = format' w (targetName target) contents
    mapM (writeTarget target) formatted

-- | Return an error if target could not be parsed or was not formatted
-- correctly.
checkTarget :: Width -> Target -> IO Result
checkTarget w target = do
    contents <- readTarget target
    return $ case format' w (targetName target) contents of
        Left err -> Left err
        Right formatted
            | formatted == contents -> Right ()
            | otherwise             -> Left $ (targetName target) ++ ": not formatted"

toTargets :: Nixfmt -> [Target]
toTargets Nixfmt{ files = [] }    = [StdioTarget]
toTargets Nixfmt{ files = paths } = map FileTarget paths

toOperation :: Nixfmt -> Target -> IO Result
toOperation Nixfmt{ width = w, check = True } = checkTarget w
toOperation Nixfmt{ width = w } = formatTarget w

toWriteError :: Nixfmt -> String -> IO ()
toWriteError Nixfmt{ quiet = False } = hPutStrLn stderr
toWriteError Nixfmt{ quiet = True } = const $ return ()

toJobs :: Nixfmt -> [IO Result]
toJobs opts = map (filterTarget $ toOperation opts) $ toTargets opts

-- TODO: Efficient parallel implementation. This is just a sequential stub.
-- This was originally implemented using parallel-io, but it gave a factor two
-- overhead.
doParallel :: [IO a] -> IO [a]
doParallel = sequence

errorWriter :: (String -> IO ()) -> Chan (Maybe String) -> Chan () -> IO ()
errorWriter doWrite chan done = do
    item <- readChan chan
    case item of
         Nothing  -> return ()
         Just msg -> doWrite msg >> errorWriter doWrite chan done
    writeChan done ()

writeErrorBundle :: Chan (Maybe String) -> Result -> IO Result
writeErrorBundle chan result = do
    case result of
         Right () -> return ()
         Left err -> writeChan chan $ Just err
    return result

-- | Run a list of jobs and write errors to stderr without interleaving them.
runJobs :: (String -> IO ()) -> [IO Result] -> IO [Result]
runJobs writeError jobs = do
    errChan <- newChan
    doneChan <- newChan
    _ <- forkIO $ errorWriter writeError errChan doneChan
    results <- doParallel $ map (>>= writeErrorBundle errChan) jobs
    writeChan errChan Nothing
    _ <- readChan doneChan
    return results

main :: IO ()
main = withUtf8StdHandles $ do
    _ <- installHandler keyboardSignal
            (Catch (exitImmediately $ ExitFailure 2)) Nothing
    opts <- cmdArgs options
    results <- runJobs (toWriteError opts) (toJobs opts)
    case lefts results of
         [] -> exitSuccess
         _  -> exitFailure
