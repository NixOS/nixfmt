{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent.ParallelIO.Local
import Data.Either
import qualified Data.Text.IO as TextIO
import Data.Traversable
import GHC.Conc (numCapabilities)
import System.Console.CmdArgs
import System.Exit
import System.IO

import Nixfmt

data Options = Options
    { files :: [FilePath]
    , width :: Int
    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    { files = def &= args &= typ "FILES/DIRS"
    , width = def &= opt (80 :: Int) &= help "Maximum width of the formatted file"
    } &= help "Format Nix source code"

formatStdio :: Int -> IO (Either ParseErrorBundle ())
formatStdio w = do
    contents <- TextIO.getContents
    formatIO w "<stdin>" contents stdout

formatFile :: Int -> FilePath -> IO (Either ParseErrorBundle ())
formatFile w path = do
    contents <- TextIO.readFile path
    withFile path WriteMode $ formatIO w path contents

doParallel :: [IO a] -> IO [a]
doParallel = withPool numCapabilities . flip parallelInterleaved

formatFiles :: Int -> [FilePath] -> IO [Either ParseErrorBundle ()]
formatFiles w = doParallel . map (formatFile w)

main :: IO ()
main = do
    opts <- cmdArgs options
    results <- case files opts of
                 [] -> pure <$> formatStdio (width opts)
                 _  -> formatFiles (width opts) (files opts)
    case lefts results of
         []     -> exitSuccess
         errors -> for errors (hPutStr stderr . errorBundlePretty) *> exitFailure
