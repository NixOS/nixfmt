{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}

module Main where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Data.Either (lefts)
import Data.Text (Text)
import Data.Version (showVersion)
import GHC.IO.Encoding (utf8)
import Paths_nixfmt (version)
import System.Console.CmdArgs
  (Data, Typeable, args, cmdArgs, help, summary, typ, (&=))
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.IO (hPutStrLn, hSetEncoding, stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler(..), installHandler, keyboardSignal)

import qualified Data.Text.IO as TextIO (getContents, hPutStr, putStr)

import qualified Nixfmt
import System.IO.Atomic (withOutputFile)
import System.IO.Utf8 (readFileUtf8, withUtf8StdHandles)

type Result = Either String ()
type Width = Int

data Nixfmt = Nixfmt
    { files :: [FilePath]
    , width :: Width
    , check :: Bool
    , quiet :: Bool
    , verify :: Bool
    } deriving (Show, Data, Typeable)

options :: Nixfmt
options =
  let defaultWidth = 80
      addDefaultHint value message =
        message ++ "\n[default: " ++ show value ++ "]"
   in Nixfmt
        { files = [] &= args &= typ "FILES"
        , width =
            defaultWidth &=
            help (addDefaultHint defaultWidth "Maximum width in characters")
        , check = False &= help "Check whether files are formatted"
        , quiet = False &= help "Do not report errors"
        , verify =
            False &=
            help
              "Check that the output parses and formats the same as the input"
        } &=
      summary ("nixfmt v" ++ showVersion version) &=
      help "Format Nix source code"

data Target = Target
    { tDoRead :: IO Text
    , tPath :: FilePath
    , tDoWrite :: Text -> IO ()
    }

formatTarget :: Formatter -> Target -> IO Result
formatTarget format Target{tDoRead, tPath, tDoWrite} = do
    contents <- tDoRead
    let formatted = format tPath contents
    mapM tDoWrite formatted

-- | Return an error if target could not be parsed or was not formatted
-- correctly.
checkTarget :: Formatter -> Target -> IO Result
checkTarget format Target{tDoRead, tPath} = do
    contents <- tDoRead
    return $ case format tPath contents of
        Left err -> Left err
        Right formatted
            | formatted == contents -> Right ()
            | otherwise             -> Left $ tPath ++ ": not formatted"

stdioTarget :: Target
stdioTarget = Target TextIO.getContents "<stdin>" TextIO.putStr

fileTarget :: FilePath -> Target
fileTarget path = Target (readFileUtf8 path) path atomicWriteFile
  where
    atomicWriteFile t = withOutputFile path $ \h -> do
      hSetEncoding h utf8
      TextIO.hPutStr h t

checkFileTarget :: FilePath -> Target
checkFileTarget path = Target (readFileUtf8 path) path (const $ pure ())

toTargets :: Nixfmt -> [Target]
toTargets Nixfmt{ files = [] }    = [stdioTarget]
toTargets Nixfmt{ files = ["-"] } = [stdioTarget]
toTargets Nixfmt{ check = False, files = paths } = map fileTarget paths
toTargets Nixfmt{ check = True, files = paths } = map checkFileTarget paths

type Formatter = FilePath -> Text -> Either String Text

toFormatter :: Nixfmt -> Formatter
toFormatter Nixfmt{ width, verify = True  } = Nixfmt.formatVerify width
toFormatter Nixfmt{ width, verify = False } = Nixfmt.format width

type Operation = Formatter -> Target -> IO Result

toOperation :: Nixfmt -> Operation
toOperation Nixfmt{ check = True } = checkTarget
toOperation Nixfmt{ } = formatTarget

toWriteError :: Nixfmt -> String -> IO ()
toWriteError Nixfmt{ quiet = False } = hPutStrLn stderr
toWriteError Nixfmt{ quiet = True } = const $ return ()

toJobs :: Nixfmt -> [IO Result]
toJobs opts = map (toOperation opts $ toFormatter opts) $ toTargets opts

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
