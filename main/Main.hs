{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.ByteString.Char8 (unpack)
import Data.Either (lefts)
import Data.FileEmbed
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO (getContents, hPutStr, putStr)
import Data.Version (showVersion)
import GHC.IO.Encoding (utf8)
import qualified Nixfmt
import Nixfmt.Predoc (layout)
import Paths_nixfmt (version)
import System.Console.CmdArgs (
  Data,
  Typeable,
  args,
  cmdArgs,
  help,
  summary,
  typ,
  (&=),
 )
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, hSetEncoding, stderr)
import System.IO.Atomic (withOutputFile)
import System.IO.Utf8 (readFileUtf8, withUtf8StdHandles)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal)

type Result = Either String ()

type Width = Int

data Nixfmt = Nixfmt
  { files :: [FilePath],
    width :: Width,
    check :: Bool,
    quiet :: Bool,
    strict :: Bool,
    verify :: Bool,
    ast :: Bool
  }
  deriving (Show, Data, Typeable)

versionFromFile :: String
versionFromFile = maybe (showVersion version) unpack $(embedFileIfExists ".version")

options :: Nixfmt
options =
  let defaultWidth = 100
      addDefaultHint value message =
        message ++ "\n[default: " ++ show value ++ "]"
  in Nixfmt
      { files = [] &= args &= typ "FILES",
        width =
          defaultWidth
            &= help (addDefaultHint defaultWidth "Maximum width in characters"),
        check = False &= help "Check whether files are formatted without modifying them",
        quiet = False &= help "Do not report errors",
        strict = False &= help "Enable a stricter formatting mode that isn't influenced as much by how the input is formatted",
        verify =
          False
            &= help
              "Apply sanity checks on the output after formatting",
        ast =
          False
            &= help
              "Pretty print the internal AST, only for debugging"
      }
      &= summary ("nixfmt " ++ versionFromFile)
      &= help "Format Nix source code"

data Target = Target
  { tDoRead :: IO Text,
    tPath :: FilePath,
    -- The bool is true when the formatted file differs from the input
    tDoWrite :: Bool -> Text -> IO ()
  }

-- | Recursively collect nix files in a directory
collectNixFiles :: FilePath -> StateT Bool IO [FilePath]
collectNixFiles path = do
  dir <- lift $ doesDirectoryExist path
  if
    | dir -> do
        warnDeprecated
        files <- lift $ listDirectory path
        concat <$> mapM collectNixFiles ((path </>) <$> files)
    | ".nix" `isSuffixOf` path -> pure [path]
    | otherwise -> do
        warnDeprecated
        pure []
  where
    warnDeprecated = do
      warningPrinted <- get
      -- We don't want to print the warning more than once
      unless warningPrinted $ do
        lift $ hPutStrLn stderr $ "\ESC[33mPassing directories or non-Nix files (such as \"" <> path <> "\") is deprecated and will be unsupported soon, please use https://treefmt.com/ instead, e.g. via https://github.com/numtide/treefmt-nix\ESC[0m"
        put True

-- | Recursively collect nix files in a list of directories
collectAllNixFiles :: [FilePath] -> IO [FilePath]
collectAllNixFiles paths =
  -- The state represents whether a deprecation warning was already printed for a directory being passed
  flip evalStateT False $ concat <$> mapM collectNixFiles paths

formatTarget :: Formatter -> Target -> IO Result
formatTarget format Target{tDoRead, tPath, tDoWrite} = do
  contents <- tDoRead
  let formatResult = format tPath contents
  mapM (\formatted -> tDoWrite (formatted /= contents) formatted) formatResult

-- | Return an error if target could not be parsed or was not formatted
-- correctly.
checkTarget :: Formatter -> Target -> IO Result
checkTarget format Target{tDoRead, tPath} = do
  contents <- tDoRead
  return $ case format tPath contents of
    Left err -> Left err
    Right formatted
      | formatted == contents -> Right ()
      | otherwise -> Left $ tPath ++ ": not formatted"

stdioTarget :: Target
stdioTarget = Target TextIO.getContents "<stdin>" (const TextIO.putStr)

fileTarget :: FilePath -> Target
fileTarget path = Target (readFileUtf8 path) path atomicWriteFile
  where
    atomicWriteFile True t = withOutputFile path $ \h -> do
      hSetEncoding h utf8
      TextIO.hPutStr h t
    -- Don't do anything if the file is already formatted
    atomicWriteFile False _ = mempty

checkFileTarget :: FilePath -> Target
checkFileTarget path = Target (readFileUtf8 path) path (const $ const $ pure ())

toTargets :: Nixfmt -> IO [Target]
toTargets Nixfmt{files = []} = pure [stdioTarget]
toTargets Nixfmt{files = ["-"]} = pure [stdioTarget]
toTargets Nixfmt{check = False, files = paths} = map fileTarget <$> collectAllNixFiles paths
toTargets Nixfmt{check = True, files = paths} = map checkFileTarget <$> collectAllNixFiles paths

type Formatter = FilePath -> Text -> Either String Text

toFormatter :: Nixfmt -> Formatter
toFormatter Nixfmt{ast = True} = Nixfmt.printAst
toFormatter Nixfmt{width, verify = True, strict} = Nixfmt.formatVerify (layout width strict)
toFormatter Nixfmt{width, verify = False, strict} = Nixfmt.format (layout width strict)

type Operation = Formatter -> Target -> IO Result

toOperation :: Nixfmt -> Operation
toOperation Nixfmt{check = True} = checkTarget
toOperation Nixfmt{} = formatTarget

toWriteError :: Nixfmt -> String -> IO ()
toWriteError Nixfmt{quiet = False} = hPutStrLn stderr
toWriteError Nixfmt{quiet = True} = const $ return ()

toJobs :: Nixfmt -> IO [IO Result]
toJobs opts = map (toOperation opts $ toFormatter opts) <$> toTargets opts

-- TODO: Efficient parallel implementation. This is just a sequential stub.
-- This was originally implemented using parallel-io, but it gave a factor two
-- overhead.
doParallel :: [IO a] -> IO [a]
doParallel = sequence

errorWriter :: (String -> IO ()) -> Chan (Maybe String) -> Chan () -> IO ()
errorWriter doWrite chan done = do
  item <- readChan chan
  case item of
    Nothing -> return ()
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
  _ <-
    installHandler
      keyboardSignal
      (Catch (exitImmediately $ ExitFailure 2))
      Nothing
  opts <- cmdArgs options
  results <- runJobs (toWriteError opts) =<< toJobs opts
  case lefts results of
    [] -> exitSuccess
    _ -> exitFailure
