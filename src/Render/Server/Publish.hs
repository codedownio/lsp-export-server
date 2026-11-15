module Render.Server.Publish (
  publish
  , hashFile
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.LSP.Protocol.Types (Int32)
import Render.Command
import Render.Protocol
import Render.Server.Env
import qualified Render.Xdelta as Xdelta
import System.Directory
import UnliftIO.STM


-- | Nothing when the output is unchanged
publish :: ServerEnv -> Task -> RenderResult -> Maybe PageBox -> [FilePath] -> Maybe Int32 -> Double -> IO (Maybe Text)
publish env task (RenderResult {..}) position overlayPaths version ms = do
  resync <- atomically (swapTVar (taskResync task) False)
  lastOutput <- readTVarIO (taskLast task)
  encoded <- case lastOutput of
    Just (_, baseBytes) | not resync, baseBytes == resultBytes -> return Nothing
    Just (baseId, baseBytes) | not resync -> Xdelta.encode resultBytes baseBytes >>= \case
      Right patch -> return (Just (EncodingXdelta3, Just baseId, patch))
      Left err -> do
        logLine [i|xdelta3 failed, sending a full output: #{err}|]
        return (Just (EncodingFull, Nothing, resultBytes))
    _ -> return (Just (EncodingFull, Nothing, resultBytes))

  let diskDeps = filter (`notElem` (taskEntryPath task : overlayPaths)) resultDeps
  hashes <- M.fromList <$> mapM (\p -> (p,) <$> hashFile p) diskDeps
  atomically $ do
    writeTVar (taskDeps task) (S.fromList (taskEntryPath task : resultDeps))
    writeTVar (taskDepHashes task) hashes
  case encoded of
    Nothing -> return Nothing
    Just (encoding, baseId, bytes) -> do
      renderId <- atomically $ stateTVar (taskNextRenderId task) (\n -> (n, n + 1))
      atomically $ writeTVar (taskLast task) (Just (renderId, resultBytes))
      send env methodOutput $ OutputParams {
        outputTaskId = taskId task
        , outputRenderId = renderId
        , outputBaseRenderId = baseId
        , outputDocumentVersion = version
        , outputFiles = [OutputFile resultFileName encoding (T.decodeUtf8 (B64.encode bytes)) (B.length resultBytes)]
        , outputDeps = diskDeps
        , outputRenderMs = ms
        , outputTimings = resultTimings
        , outputPosition = position
        }
      return $ Just $ case encoding of
        EncodingFull -> [i|sent the full #{B.length bytes}B output as render #{renderId}|]
        EncodingXdelta3 -> [i|output changed, sent a #{B.length bytes}B diff as render #{renderId}|]

hashFile :: FilePath -> IO (Maybe B.ByteString)
hashFile path = doesFileExist path >>= \case
  False -> return Nothing
  True -> Just . SHA256.hash <$> B.readFile path
