{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (forM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Bifunctor (first)
import Data.ByteString.Char8 (unpack)
import Data.Either (lefts)
import Data.FileEmbed
import Data.List (intersperse, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO (getContents, hGetContents, hPutStr, putStr)
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
import System.IO (Handle, hGetContents, hPutStrLn, hSetEncoding, stderr)
import System.IO.Atomic (withOutputFile)
import System.IO.Utf8 (readFileUtf8, withUtf8StdHandles)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, proc, waitForProcess)

type Result = Either String ()

type Width = Int

data Nixfmt = Nixfmt
  { files :: [FilePath],
    width :: Width,
    indent :: Int,
    check :: Bool,
    mergetool :: Bool,
    quiet :: Bool,
    strict :: Bool,
    verify :: Bool,
    ast :: Bool,
    filename :: Maybe FilePath,
    ir :: Bool
  }
  deriving (Show, Data, Typeable)

versionFromFile :: String
versionFromFile = maybe (showVersion version) unpack $(embedFileIfExists ".version")

options :: Nixfmt
options =
  let defaultWidth = 100
      defaultIndent = 2
      addDefaultHint value message =
        message ++ "\n[default: " ++ show value ++ "]"
  in Nixfmt
      { files = [] &= args &= typ "FILES",
        width =
          defaultWidth
            &= help (addDefaultHint defaultWidth "Maximum width in characters"),
        indent = defaultIndent &= help (addDefaultHint defaultIndent "Number of spaces to use for indentation"),
        check = False &= help "Check whether files are formatted without modifying them",
        mergetool = False &= help "Whether to run in git mergetool mode, see https://github.com/NixOS/nixfmt?tab=readme-ov-file#git-mergetool for more info",
        quiet = False &= help "Do not report errors",
        strict = False &= help "Enable a stricter formatting mode that isn't influenced as much by how the input is formatted",
        verify =
          False
            &= help
              "Apply sanity checks on the output after formatting",
        ast =
          False
            &= help
              "Pretty print the internal AST, only for debugging",
        filename =
          Nothing
            &= help
              "The filename to display when the file input is given through stdin.\n\
              \Useful for tools like editors and autoformatters that wish to use Nixfmt without providing it direct file access, while still providing context to where the file is.",
        ir =
          False
            &= help
              "Pretty print the internal intermediate representation, only for debugging"
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
        lift $ hPutStrLn stderr $ "\ESC[33mPassing directories or non-Nix files (such as \"" <> path <> "\") is deprecated and will be unsupported soon. Please use the `pkgs.nixfmt-tree` wrapper instead, or https://github.com/numtide/treefmt-nix for more flexibility\ESC[0m"
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

stdioTarget :: Maybe FilePath -> Target
stdioTarget filename = Target TextIO.getContents (fromMaybe "<stdin>" filename) (const TextIO.putStr)

fileTarget :: FilePath -> Target
fileTarget path = Target (readFileUtf8 path) path atomicWriteFile
  where
    atomicWriteFile True t = withOutputFile path $ \h -> do
      hSetEncoding h utf8
      TextIO.hPutStr h t
    -- Don't do anything if the file is already formatted
    atomicWriteFile False _ = mempty

-- | Writes to a (potentially non-existent) file path, but reads from a potentially separate handle
copyTarget :: Handle -> FilePath -> Target
copyTarget from to = Target (TextIO.hGetContents from) to atomicWriteFile
  where
    atomicWriteFile _ t = withOutputFile to $ \h -> do
      hSetEncoding h utf8
      TextIO.hPutStr h t

checkFileTarget :: FilePath -> Target
checkFileTarget path = Target (readFileUtf8 path) path (const $ const $ pure ())

toTargets :: Nixfmt -> IO [Target]
toTargets Nixfmt{files = [], filename} = pure [stdioTarget filename]
toTargets Nixfmt{files = ["-"], filename} = pure [stdioTarget filename]
toTargets Nixfmt{check = False, files = paths} = map fileTarget <$> collectAllNixFiles paths
toTargets Nixfmt{check = True, files = paths} = map checkFileTarget <$> collectAllNixFiles paths

type Formatter = FilePath -> Text -> Either String Text

toFormatter :: Nixfmt -> Formatter
toFormatter Nixfmt{ast = True} = Nixfmt.printAst
toFormatter Nixfmt{ir = True} = Nixfmt.printIR
toFormatter Nixfmt{width, indent, verify = True, strict} = Nixfmt.formatVerify (layout width indent strict)
toFormatter Nixfmt{width, indent, verify = False, strict} = Nixfmt.format (layout width indent strict)

type Operation = Formatter -> Target -> IO Result

toOperation :: Nixfmt -> Operation
toOperation Nixfmt{check = True} = checkTarget
toOperation Nixfmt{} = formatTarget

toWriteError :: Nixfmt -> String -> IO ()
toWriteError Nixfmt{quiet = False} = hPutStrLn stderr
toWriteError Nixfmt{quiet = True} = const $ return ()

-- | `git mergetool` mode, which rejects all non-\`.nix\` files, while for \`.nix\` files it simply
-- - Calls `nixfmt` on its first three inputs (the BASE, LOCAL and REMOTE versions to merge)
-- - Runs `git merge-file` on the same inputs
-- - Runs `nixfmt` on the result and stores it in the path given in the fourth argument (the MERGED file)
mergeToolJob :: Nixfmt -> IO Result
mergeToolJob opts@Nixfmt{files = [base, local, remote, merged]} = runExceptT $ do
  let formatter = toFormatter opts
      joinResults :: [Result] -> Result
      joinResults xs = case lefts xs of
        [] -> Right ()
        ls -> Left (mconcat (intersperse "\n" ls))
      inputs =
        [ ("base", base),
          ("local", local),
          ("remote", remote)
        ]

  unless (".nix" `isSuffixOf` merged) $
    throwE ("Skipping non-Nix file " ++ merged)

  ExceptT $
    joinResults
      <$> forM
        inputs
        ( \(name, path) -> do
            first (<> "pre-formatting the " <> name <> " version failed")
              <$> formatTarget formatter (fileTarget path)
        )

  (_, Just out, _, process) <- do
    lift $
      createProcess
        (proc "git" ["merge-file", "--stdout", local, base, remote])
          { std_out = CreatePipe
          }

  lift (waitForProcess process) >>= \case
    ExitFailure code -> do
      output <- lift $ hGetContents out
      throwE $ output <> "`git merge-file` failed with exit code " <> show code <> "\n"
    ExitSuccess -> return ()

  ExceptT $ formatTarget formatter (copyTarget out merged)
mergeToolJob _ = return $ Left "--mergetool mode expects exactly 4 file arguments ($BASE, $LOCAL, $REMOTE, $MERGED)"

toJobs :: Nixfmt -> IO [IO Result]
toJobs opts@Nixfmt{mergetool = False} = map (toOperation opts $ toFormatter opts) <$> toTargets opts
toJobs opts@Nixfmt{mergetool = True} = return [mergeToolJob opts]

writeErrorBundle :: (String -> IO ()) -> Result -> IO Result
writeErrorBundle doWrite result = do
  case result of
    Right () -> return ()
    Left err -> doWrite err
  return result

-- | Run a list of jobs and write errors to stderr without interleaving them.
runJobs :: (String -> IO ()) -> [IO Result] -> IO [Result]
runJobs writeError = mapM (>>= writeErrorBundle writeError)

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
