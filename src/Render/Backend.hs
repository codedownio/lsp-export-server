module Render.Backend (
  Backend(..)
  , RenderRequest(..)
  , RenderResult(..)
  , RenderFailure(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Render.Protocol (LogStream, Timing)
import UnliftIO (Exception)


data RenderRequest = RenderRequest {
  -- | The real document, as an absolute path; the render's working directory is its directory
  requestEntryPath :: FilePath
  , requestEntryText :: Text
  -- | Other open buffers (absolute real paths) that take priority over their disk copies
  , requestOverlays :: [(FilePath, Text)]
  -- | Private scratch space for this task, kept across renders
  , requestWorkDir :: FilePath
  , requestOptions :: Map Text Text
  -- | A full build (cross-references settled) rather than a quick single pass
  , requestSettle :: Bool
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

data Backend = Backend {
  backendName :: Text
  , backendRender :: RenderRequest -> IO RenderResult
  }
