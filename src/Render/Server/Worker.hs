module Render.Server.Worker (
  runWorker
  , locatePosition
  ) where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (check)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS
import Render.Command
import Render.Protocol
import Render.Server.Env
import Render.Server.Publish
import UnliftIO (SomeException, fromException, race, try)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM


runWorker :: ServerEnv -> Task -> IO ()
runWorker env task = forever $ do
  atomically $ do
    dirty <- readTVar (taskDirty task)
    paused <- readTVar (taskPaused task)
    check (dirty && not paused)
  atomically $ writeTVar (taskDirty task) False
  threadDelay (taskDebounceMs task * 1000)
  readTVarIO (taskDirty task) >>= \case
    True -> return ()
    False -> renderOnce env task PhaseQuick >>= \case
      False -> return ()
      True -> case programSettle (envCommand env) of
        Nothing -> return ()
        Just _ -> quietFor settleQuietMs >>= \case
          False -> return ()
          True -> void (renderOnce env task PhaseSettle)
  where
    quietFor ms = race (threadDelay (ms * 1000)) (atomically (readTVar (taskDirty task) >>= check)) >>= \case
      Left () -> return True
      Right () -> return False

settleQuietMs :: Int
settleQuietMs = 1500

-- | False when superseded by an edit or failed
renderOnce :: ServerEnv -> Task -> Phase -> IO Bool
renderOnce env task phase = do
  (entryText, version, overlays) <- snapshotBuffers env task
  reasons <- atomically (swapTVar (taskReasons task) [])
  edit <- atomically $ case phase of
    PhaseQuick -> do
      e <- swapTVar (taskEdit task) Nothing
      writeTVar (taskRenderedEdit task) e
      return e
    PhaseSettle -> readTVar (taskRenderedEdit task)
  let reason = case reasons of
        [] -> Nothing
        _ -> Just (T.intercalate ", " reasons)
  let label = case phase of
        PhaseQuick -> "quick render" :: Text
        PhaseSettle -> "settle pass"
  -- The settle pass logs only when it fails or changes the output
  buffered <- newTVarIO ([] :: [(LogStream, Text)])
  let logFn stream text = case phase of
        PhaseQuick -> sendLog env task stream text False
        PhaseSettle -> atomically (modifyTVar' buffered ((stream, text) :))
  let marker text = logFn LogStdout ("── " <> text)
  let flush = do
        lines' <- reverse <$> atomically (swapTVar buffered [])
        forM_ lines' $ \(stream, text) -> sendLog env task stream text False
  case phase of
    PhaseQuick -> do
      sendStatus env task StatusRendering phase reason Nothing Nothing []
      sendLog env task LogStdout "" True
      marker [i|#{label}: #{fromMaybe "no reason recorded" reason}|]
    PhaseSettle -> marker [i|#{label} after #{settleQuietMs}ms without edits|]
  start <- getCurrentTime
  let request = RenderRequest {
        requestEntryPath = taskEntryPath task
        , requestEntryText = entryText
        , requestOverlays = overlays
        , requestWorkDir = taskWorkDir task
        , requestOptions = taskOptions task
        , requestSettle = phase == PhaseSettle
        , requestEpoch = taskEpoch task
        , requestLog = logFn
        }
  outcome <- race (atomically (readTVar (taskDirty task) >>= check)) (try (render (envCommand env) request))
  end <- getCurrentTime
  let ms = realToFrac (diffUTCTime end start) * 1000 :: Double
  case outcome of
    Left () -> do
      marker [i|#{label} cancelled after #{showMs ms}: a new edit arrived|]
      return False
    Right (Left (e :: SomeException)) -> do
      let (code, msg) = case fromException e of
            Just (RenderFailure c m) -> (c, m)
            Nothing -> (-1, T.pack (show e))
      logFn LogStderr msg
      marker [i|#{label} failed after #{showMs ms}|]
      flush
      sendStatus env task StatusErrored phase reason (Just code) (Just ms) []
      return False
    Right (Right result) -> do
      positions <- forM edit (uncurry (locatePosition env task)) >>= \case
        Just (Left err) -> do
          logLine [i|locating the edit in #{taskId task} failed: #{err}|]
          return []
        Just (Right boxes) -> return boxes
        Nothing -> return []
      sent <- publish env task result positions (map fst overlays) version ms
      marker [i|#{label} done in #{showMs ms}: #{fromMaybe "output unchanged, nothing sent" sent}|]
      when (isJust sent) flush
      sendStatus env task StatusReady phase reason Nothing (Just ms) (resultTimings result)
      return True

locatePosition :: ServerEnv -> Task -> FilePath -> Position -> IO (Either Text [PageBox])
locatePosition env task path (Position line character) = case programLocate (envCommand env) of
  Nothing -> return (Right [])
  Just exe -> do
    source <- workPathFor (taskEntryPath task) (taskWorkDir task) path
    locate exe (taskEntryPath task) (taskWorkDir task) (taskFormat task) source (fromIntegral line + 1) (fromIntegral character + 1)

showMs :: Double -> Text
showMs ms = T.pack (show (round ms :: Int)) <> "ms"

snapshotBuffers :: ServerEnv -> Task -> IO (Text, Maybe Int32, [(FilePath, Text)])
snapshotBuffers env task = readTVarIO (envLsp env) >>= \case
  Nothing -> fromDisk
  Just lspEnv -> runLspT lspEnv $ do
    vfs <- getVirtualFiles
    let overlays = [ (p, virtualFileText vf)
                   | (uri, Open vf) <- M.toList (vfs ^. vfsMap)
                   , uri /= taskEntryUri task
                   , Just p <- [uriToFilePath (fromNormalizedUri uri)] ]
    getVirtualFile (taskEntryUri task) >>= \case
      Just vf -> return (virtualFileText vf, Just (virtualFileVersion vf), overlays)
      Nothing -> do
        (text, v, _) <- liftIO fromDisk
        return (text, v, overlays)
  where
    fromDisk = do
      text <- T.readFile (taskEntryPath task)
      return (text, Nothing, [])
