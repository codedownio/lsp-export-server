module Render.Backend.Pandoc (
  pandocBackend
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Render.Backend
import Render.Backend.Latex (compileTex, prepareWorkDir)
import Render.Process
import Render.Protocol (LogStream(..), Timing(..))
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import UnliftIO (throwIO)


-- Options: @to@ (pandoc writer; @pdf@ means @latex@), @format@ (output extension), @from@ (reader
-- for Markdown input), @pandoc_args@ (extra arguments, space-separated), and the LaTeX backend's
-- @engine@ for the LaTeX-based writers.
pandocBackend :: Backend
pandocBackend = Backend {
  backendName = "pandoc"
  , backendRender = render
  }

render :: RenderRequest -> IO RenderResult
render req@(RenderRequest {..}) = do
  srcDir <- prepareWorkDir req
  let entryFile = takeFileName requestEntryPath
      job = dropExtension entryFile
      outDir = requestWorkDir </> "out"
      opt k = M.lookup k requestOptions
      to = fromMaybe "html" (opt "to")
      writer = case to of
        "pdf" -> "latex"
        other -> other
      format = T.unpack (fromMaybe (defaultExtension writer) (opt "format"))
      reader = case takeExtension entryFile of
        ".md" -> ["-f", T.unpack (fromMaybe defaultMarkdown (opt "from"))]
        _ -> []
      extra = maybe [] (map T.unpack . T.words) (opt "pandoc_args")
      viaLatex = writer `elem` ["latex", "beamer"]
      output = case viaLatex of
        True -> srcDir </> job <.> "tex"
        False -> outDir </> job <.> format
      args = reader <> ["-t", T.unpack writer, "--standalone"] <> extra <> [srcDir </> entryFile, "-o", output]

  env <- (("LC_ALL", "C") :) . filter ((/= "LC_ALL") . fst) <$> getEnvironment
  requestLog LogStdout (T.pack (unwords ("pandoc" : args)))
  start <- getCurrentTime
  code <- runCapturing (takeDirectory requestEntryPath) env "pandoc" args $ \isOut line ->
    requestLog (if isOut then LogStdout else LogStderr) line
  end <- getCurrentTime
  let ms = realToFrac (diffUTCTime end start) * 1000 :: Double
      pandocTiming = Timing "pandoc" ms

  case code of
    ExitFailure n -> throwIO (RenderFailure n [i|pandoc exited with code #{n}|])
    ExitSuccess -> return ()

  case viaLatex of
    True -> do
      result <- compileTex req output
      return result { resultTimings = pandocTiming : resultTimings result }
    False -> do
      bytes <- B.readFile output
      return RenderResult {
        resultFileName = T.pack (job <.> format)
        , resultBytes = bytes
        , resultDeps = []
        , resultTimings = [pandocTiming]
        }

defaultMarkdown :: Text
defaultMarkdown = "markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart"

defaultExtension :: Text -> Text
defaultExtension = \case
  "latex" -> "pdf"
  "beamer" -> "pdf"
  "docx" -> "docx"
  "epub" -> "epub"
  "rst" -> "rst"
  "plain" -> "txt"
  _ -> "html"
