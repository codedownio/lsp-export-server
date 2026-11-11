module Render.Command (
  Programs(..)
  , RenderRequest(..)
  , RenderResult(..)
  , RenderFailure(..)
  , render
  , prepareWorkDir
  , mapDeps
  ) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (isPrefixOf, nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Render.Process
import Render.Protocol (LogStream(..), Timing(..))
import System.Directory
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import UnliftIO (Exception, throwIO)


-- | The programs that render: one for the quick pass after each edit, and optionally one for the
-- settle pass once edits stay quiet. Each runs in the document's directory with the layout and
-- options in @RENDER_*@ variables and must write @RENDER_OUTPUT@.
data Programs = Programs {
  programRender :: FilePath
  , programSettle :: Maybe FilePath
  }

data RenderRequest = RenderRequest {
  -- | The real document, as an absolute path; the render's working directory is its directory
  requestEntryPath :: FilePath
  , requestEntryText :: Text
  -- | Other open buffers (absolute real paths) that take priority over their disk copies
  , requestOverlays :: [(FilePath, Text)]
  -- | Private scratch space for this task, kept across renders
  , requestWorkDir :: FilePath
  , requestOptions :: Map Text Text
  -- | The settle pass rather than the quick one
  , requestSettle :: Bool
  -- | Unix time for the output's timestamps, kept fixed so unchanged input gives unchanged bytes
  , requestEpoch :: Integer
  , requestLog :: LogStream -> Text -> IO ()
  }

data RenderResult = RenderResult {
  resultFileName :: Text
  , resultBytes :: ByteString
  -- | Absolute paths of every file read, excluding the scratch space and the Nix store
  , resultDeps :: [FilePath]
  , resultTimings :: [Timing]
  }

data RenderFailure = RenderFailure {
  failureExitCode :: Int
  , failureMessage :: Text
  } deriving (Show)
instance Exception RenderFailure

-- | Lay out the work directory, run the pass's program and collect its output and dependencies.
render :: Programs -> RenderRequest -> IO RenderResult
render cmd req@(RenderRequest {..}) = do
  prepareWorkDir req
  let exe = case requestSettle of
        True -> fromMaybe (programRender cmd) (programSettle cmd)
        False -> programRender cmd
  let job = dropExtension (takeFileName requestEntryPath)
  let format = T.unpack (fromMaybe "" (M.lookup "format" requestOptions))
  let output = requestWorkDir </> "out" </> job <.> format
  let depsFile = requestWorkDir </> "deps"
  doesFileExist depsFile >>= \case
    True -> removeFile depsFile
    False -> return ()
  let vars = [
        ("RENDER_PHASE", case requestSettle of { True -> "settle"; False -> "quick" })
        , ("RENDER_DOCUMENT", requestEntryPath)
        , ("RENDER_ENTRY", requestWorkDir </> "src" </> takeFileName requestEntryPath)
        , ("RENDER_JOB", job)
        , ("RENDER_FORMAT", format)
        , ("RENDER_OUTPUT", output)
        , ("RENDER_WORK", requestWorkDir)
        , ("RENDER_SRC", requestWorkDir </> "src")
        , ("RENDER_OVERLAY", requestWorkDir </> "overlay")
        , ("RENDER_OUT", requestWorkDir </> "out")
        , ("RENDER_DEPS", depsFile)
        , ("RENDER_EPOCH", show requestEpoch)
        ] <> [("RENDER_OPT_" <> T.unpack k, T.unpack v) | (k, v) <- M.toList requestOptions]
  env <- (vars <>) . filter ((`notElem` map fst vars) . fst) <$> getEnvironment

  let entryDir = takeDirectory requestEntryPath
  start <- getCurrentTime
  code <- runCapturing entryDir env exe [] $ \isOut line -> requestLog (logStream isOut) line
  end <- getCurrentTime
  let ms = realToFrac (diffUTCTime end start) * 1000 :: Double
  let name = takeFileName exe

  case code of
    ExitFailure n -> throwIO (RenderFailure n [i|#{name} exited with code #{n}|])
    ExitSuccess -> return ()
  bytes <- doesFileExist output >>= \case
    True -> B.readFile output
    False -> throwIO (RenderFailure 1 [i|#{name} exited with code 0 but wrote no #{output}|])
  deps <- doesFileExist depsFile >>= \case
    True -> map T.unpack . filter (not . T.null) . T.lines <$> T.readFile depsFile
    False -> return []
  return RenderResult {
    resultFileName = T.pack (job <.> format)
    , resultBytes = bytes
    , resultDeps = mapDeps entryDir requestEntryPath requestWorkDir deps
    , resultTimings = [Timing (T.pack name) ms]
    }

logStream :: Bool -> LogStream
logStream = \case
  True -> LogStdout
  False -> LogStderr

-- | Lay out the work directory: the entry buffer under @src/@, the other open buffers under
-- @overlay/@ at their paths relative to the entry's directory, and an @out/@ for the output.
prepareWorkDir :: RenderRequest -> IO ()
prepareWorkDir (RenderRequest {..}) = do
  let srcDir = requestWorkDir </> "src"
  forM_ [srcDir, requestWorkDir </> "out"] (createDirectoryIfMissing True)
  T.writeFile (srcDir </> takeFileName requestEntryPath) requestEntryText

  let overlayDir = requestWorkDir </> "overlay"
  doesDirectoryExist overlayDir >>= \case
    True -> removeDirectoryRecursive overlayDir
    False -> return ()
  createDirectoryIfMissing True overlayDir
  forM_ requestOverlays $ \(path, text) -> do
    let rel = makeRelative (takeDirectory requestEntryPath) path
    unless (isAbsolute rel || ".." `isPrefixOf` rel) $ do
      createDirectoryIfMissing True (takeDirectory (overlayDir </> rel))
      T.writeFile (overlayDir </> rel) text

-- | The files a render read, as the program reported them (absolute, or relative to the document's
-- directory): the work directory's copies mapped back to the documents they stand for, its other
-- files and the Nix store left out.
mapDeps :: FilePath -> FilePath -> FilePath -> [FilePath] -> [FilePath]
mapDeps entryDir entryPath workDir = nub . concatMap (classify . normalise . absolute)
  where
    srcDir = normalise (workDir </> "src")
    overlayDir = normalise (workDir </> "overlay")

    absolute p = case isAbsolute p of
      True -> p
      False -> entryDir </> p

    classify p
      | "/nix/store" `isPrefixOf` p = []
      | srcDir `isPrefixOf` p = [entryPath]
      | overlayDir `isPrefixOf` p = [entryDir </> makeRelative overlayDir p]
      | normalise workDir `isPrefixOf` p = []
      | otherwise = [p]
