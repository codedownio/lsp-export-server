module Render.Backend.Latex (
  latexBackend
  , prepareWorkDir
  , compileTex
  , depsFromFls
  , logStream
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as B
import Data.List (isPrefixOf, nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Render.Backend
import Render.Process
import Render.Protocol (LogStream(..), Timing(..))
import System.Directory
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import UnliftIO (throwIO)


-- TeX resolves relative paths against the working directory, so running in the document's real
-- directory makes its imports resolve from the home while the main file and the unsaved overlays
-- live in the work directory, found first through TEXINPUTS.
latexBackend :: Backend
latexBackend = Backend {
  backendName = "latex"
  , backendRender = \req -> do
      srcDir <- prepareWorkDir req
      compileTex req (srcDir </> takeFileName (requestEntryPath req))
  }

-- | Lay out the work directory: the entry buffer under @src/@, the other open buffers under
-- @overlay/@ at their paths relative to the entry's directory, and an @out/@ for the build.
-- Returns the @src/@ directory.
prepareWorkDir :: RenderRequest -> IO FilePath
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

  return srcDir

-- | Compile a @.tex@ file that lives in the work directory's @src/@: one engine pass, or a latexmk
-- build when settling. The PDF, the recorded dependencies and the timing come back.
compileTex :: RenderRequest -> FilePath -> IO RenderResult
compileTex (RenderRequest {..}) texFile = do
  let job = dropExtension (takeFileName texFile)
  let outDir = requestWorkDir </> "out"
  let overlayDir = requestWorkDir </> "overlay"
  let engine = T.unpack (fromMaybe "pdflatex" (M.lookup "engine" requestOptions))
  env <- texEnv overlayDir requestEpoch <$> getEnvironment
  let common = ["-interaction=nonstopmode", "-halt-on-error", "-file-line-error", "-recorder", "-jobname=" <> job]
  let (exe, args) = case requestSettle of
        True -> ("latexmk", [engineFlag engine, "-outdir=" <> outDir] <> common <> [texFile])
        False -> (engine, ["-output-directory=" <> outDir] <> common <> [texFile])

  requestLog LogStdout (T.pack (unwords (exe : args)))
  let entryDir = takeDirectory requestEntryPath
  start <- getCurrentTime
  code <- runCapturing entryDir env exe args $ \isOut line -> requestLog (logStream isOut) line
  end <- getCurrentTime
  let ms = realToFrac (diffUTCTime end start) * 1000 :: Double

  case code of
    ExitFailure n -> do
      logErrors (outDir </> job <.> "log") requestLog
      throwIO (RenderFailure n [i|#{exe} exited with code #{n}|])
    ExitSuccess -> return ()

  bytes <- B.readFile (outDir </> job <.> "pdf")
  fls <- doesFileExist (outDir </> job <.> "fls") >>= \case
    True -> T.readFile (outDir </> job <.> "fls")
    False -> return ""
  return RenderResult {
    resultFileName = T.pack (job <.> "pdf")
    , resultBytes = bytes
    , resultDeps = depsFromFls entryDir requestEntryPath (requestWorkDir </> "src") overlayDir requestWorkDir fls
    , resultTimings = [Timing (T.pack exe) ms]
    }

logStream :: Bool -> LogStream
logStream = \case
  True -> LogStdout
  False -> LogStderr

-- The overlay directory comes first on TEXINPUTS so open buffers shadow their disk copies; the
-- epoch pins the timestamps the engine writes into the PDF
texEnv :: FilePath -> Integer -> [(String, String)] -> [(String, String)]
texEnv overlayDir epoch baseEnv = overrides <> filter ((`notElem` map fst overrides) . fst) baseEnv
  where
    overrides = [
      ("TEXINPUTS", overlayDir <> "//:" <> fromMaybe "" (lookup "TEXINPUTS" baseEnv))
      , ("LC_ALL", "C")
      , ("SOURCE_DATE_EPOCH", show epoch)
      , ("FORCE_SOURCE_DATE", "1")
      ]

engineFlag :: String -> String
engineFlag = \case
  "xelatex" -> "-pdfxe"
  "lualatex" -> "-pdflua"
  _ -> "-pdf"

-- | The files a run read, from the recorder's @INPUT@ lines: absolute, with the work directory's
-- copies mapped back to the documents they stand for, and the Nix store left out.
depsFromFls :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> T.Text -> [FilePath]
depsFromFls entryDir entryPath srcDir overlayDir workDir fls = nub (mapMaybe dep (T.lines fls))
  where
    dep line = case T.stripPrefix "INPUT " line of
      Nothing -> Nothing
      Just (T.unpack . T.strip -> p) -> classify (normalise (absolute p))

    absolute p = case isAbsolute p of
      True -> p
      False -> entryDir </> p

    classify abs'
      | "/nix/store" `isPrefixOf` abs' = Nothing
      | normalise srcDir `isPrefixOf` abs' = Just entryPath
      | normalise overlayDir `isPrefixOf` abs' = Just (entryDir </> makeRelative (normalise overlayDir) abs')
      | normalise workDir `isPrefixOf` abs' = Nothing
      | otherwise = Just abs'

-- The engine's own account of what went wrong: the @!@ lines and the @file:line:@ errors
logErrors :: FilePath -> (LogStream -> T.Text -> IO ()) -> IO ()
logErrors logFile logFn = doesFileExist logFile >>= \case
  False -> return ()
  True -> do
    ls <- T.lines <$> T.readFile logFile
    let interesting = [l | (l, next) <- zip ls (drop 1 ls <> [""]), isError l || isError next]
    when (not (null interesting)) $ logFn LogStderr (T.unlines interesting)
  where
    isError l = "!" `T.isPrefixOf` l || T.isInfixOf ":error:" l || hasLineError l
    hasLineError l = case T.splitOn ":" l of
      (_file : lineNo : _ : _) -> T.all (`elem` ['0'..'9']) lineNo && not (T.null lineNo) && ".tex" `T.isInfixOf` l
      _ -> False
