module Render.Backend.Latex (
  latexBackend
  , prepareWorkDir
  , compileTex
  , depsFromFls
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
  let entryDir = takeDirectory requestEntryPath
      srcDir = requestWorkDir </> "src"
      overlayDir = requestWorkDir </> "overlay"
      outDir = requestWorkDir </> "out"

  forM_ [srcDir, outDir] (createDirectoryIfMissing True)
  T.writeFile (srcDir </> takeFileName requestEntryPath) requestEntryText

  doesDirectoryExist overlayDir >>= \case
    True -> removeDirectoryRecursive overlayDir
    False -> return ()
  createDirectoryIfMissing True overlayDir
  forM_ requestOverlays $ \(path, text) -> do
    let rel = makeRelative entryDir path
    unless (isAbsolute rel || ".." `isPrefixOf` rel) $ do
      createDirectoryIfMissing True (takeDirectory (overlayDir </> rel))
      T.writeFile (overlayDir </> rel) text

  return srcDir

-- | Compile a @.tex@ file that lives in the work directory's @src/@: one engine pass, or a latexmk
-- build when settling. The PDF, the recorded dependencies and the timing come back.
compileTex :: RenderRequest -> FilePath -> IO RenderResult
compileTex (RenderRequest {..}) texFile = do
  let entryDir = takeDirectory requestEntryPath
      job = dropExtension (takeFileName texFile)
      srcDir = requestWorkDir </> "src"
      overlayDir = requestWorkDir </> "overlay"
      outDir = requestWorkDir </> "out"

  let engine = T.unpack (fromMaybe "pdflatex" (M.lookup "engine" requestOptions))
  baseEnv <- getEnvironment
  let inherited = fromMaybe "" (lookup "TEXINPUTS" baseEnv)
      texinputs = overlayDir <> "//:" <> (if null inherited then "" else inherited)
      overrides = [("TEXINPUTS", texinputs), ("LC_ALL", "C"), ("SOURCE_DATE_EPOCH", show requestEpoch), ("FORCE_SOURCE_DATE", "1")]
      env = overrides <> filter ((`notElem` map fst overrides) . fst) baseEnv
      common = ["-interaction=nonstopmode", "-halt-on-error", "-file-line-error", "-recorder", "-jobname=" <> job]
      (exe, args) = case requestSettle of
        True -> ("latexmk", [engineFlag engine, "-outdir=" <> outDir] <> common <> [texFile])
        False -> (engine, ["-output-directory=" <> outDir] <> common <> [texFile])

  requestLog LogStdout (T.pack (unwords (exe : args)))
  start <- getCurrentTime
  code <- runCapturing entryDir env exe args $ \isOut line -> requestLog (if isOut then LogStdout else LogStderr) line
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
  let deps = depsFromFls entryDir requestEntryPath srcDir overlayDir requestWorkDir fls
  return RenderResult {
    resultFileName = T.pack (job <.> "pdf")
    , resultBytes = bytes
    , resultDeps = deps
    , resultTimings = [Timing (T.pack exe) ms]
    }

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
      Just (T.unpack . T.strip -> p) ->
        let abs' = normalise (if isAbsolute p then p else entryDir </> p)
        in if | "/nix/store" `isPrefixOf` abs' -> Nothing
              | normalise srcDir `isPrefixOf` abs' -> Just entryPath
              | normalise overlayDir `isPrefixOf` abs' -> Just (entryDir </> makeRelative (normalise overlayDir) abs')
              | normalise workDir `isPrefixOf` abs' -> Nothing
              | otherwise -> Just abs'

-- The engine's own account of what went wrong: the @!@ lines and the @file:line:@ errors
logErrors :: FilePath -> (LogStream -> T.Text -> IO ()) -> IO ()
logErrors logFile logFn = doesFileExist logFile >>= \case
  False -> return ()
  True -> do
    contents <- T.readFile logFile
    let ls = T.lines contents
        interesting = [l | (l, next) <- zip ls (drop 1 ls <> [""]), isError l || isError next]
    when (not (null interesting)) $ logFn LogStderr (T.unlines interesting)
  where
    isError l = "!" `T.isPrefixOf` l || T.isInfixOf ":error:" l || hasLineError l
    hasLineError l = case T.splitOn ":" l of
      (_file : lineNo : _ : _) -> T.all (`elem` ['0'..'9']) lineNo && not (T.null lineNo) && ".tex" `T.isInfixOf` l
      _ -> False
