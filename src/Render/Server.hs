{-# LANGUAGE TypeOperators #-}

module Render.Server (
  runRenderServer
  ) where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (check)
import Data.Aeson (Result(..), Value, fromJSON, toJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS
import Render.Backend
import Render.Protocol
import qualified Render.Xdelta as Xdelta
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import UnliftIO (Async, SomeException, async, cancel, fromException, race, try)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM


data Task = Task {
  taskId :: Text
  , taskEntryPath :: FilePath
  , taskEntryUri :: NormalizedUri
  , taskOptions :: Map Text Text
  , taskDebounceMs :: Int
  , taskWorkDir :: FilePath
  , taskDirty :: TVar Bool
  , taskPaused :: TVar Bool
  , taskResync :: TVar Bool
  , taskDeps :: TVar (Set FilePath)
  , taskVersion :: TVar (Maybe Int32)
  -- | The last output announced: its render id and bytes, the base for the next diff
  , taskLast :: TVar (Maybe (Int, B.ByteString))
  , taskNextRenderId :: TVar Int
  , taskWorker :: TVar (Maybe (Async ()))
  }

data ServerEnv = ServerEnv {
  envBackend :: Backend
  , envWorkRoot :: FilePath
  , envDefaultOptions :: Map Text Text
  , envTasks :: TVar (Map Text Task)
  , envLsp :: TVar (Maybe (LanguageContextEnv ()))
  }

-- | Serve on stdio until the client exits us. Returns the exit code.
runRenderServer :: Backend -> FilePath -> Map Text Text -> IO Int
runRenderServer backend workRoot defaultOpts = do
  env <- ServerEnv backend workRoot defaultOpts <$> newTVarIO mempty <*> newTVarIO Nothing
  runServer ServerDefinition {
    defaultConfig = ()
    , configSection = "codedown-render"
    , parseConfig = \_ _ -> Right ()
    , onConfigChange = const (return ())
    , doInitialize = \lspEnv _ -> do
        atomically (writeTVar (envLsp env) (Just lspEnv))
        return (Right lspEnv)
    , staticHandlers = const (handlers env)
    , interpretHandler = \lspEnv -> Iso (runLspT lspEnv) liftIO
    , options = defaultOptions {
        optTextDocumentSync = Just TextDocumentSyncOptions {
          _openClose = Just True
          , _change = Just TextDocumentSyncKind_Incremental
          , _willSave = Nothing
          , _willSaveWaitUntil = Nothing
          , _save = Nothing
          }
        , optExecuteCommandCommands = Just allCommands
        }
    }

handlers :: ServerEnv -> Handlers (LspM ())
handlers env = mconcat [
  notificationHandler SMethod_Initialized $ \_ -> return ()
  , notificationHandler SMethod_TextDocumentDidOpen $ \msg ->
      liftIO $ documentTouched env (msg ^. L.params . L.textDocument . L.uri) Nothing
  , notificationHandler SMethod_TextDocumentDidChange $ \msg ->
      liftIO $ documentTouched env (msg ^. L.params . L.textDocument . L.uri) (Just (msg ^. L.params . L.textDocument . L.version))
  , notificationHandler SMethod_TextDocumentDidClose $ \_ -> return ()
  , notificationHandler SMethod_TextDocumentDidSave $ \_ -> return ()
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> return ()
  , notificationHandler SMethod_CancelRequest $ \_ -> return ()
  -- The lsp library always advertises semantic tokens; answer with nothing rather than an error
  , requestHandler SMethod_TextDocumentSemanticTokensFull $ \_ respond -> respond (Right (InR Null))
  , requestHandler SMethod_WorkspaceExecuteCommand $ \req respond -> do
      let cmd = req ^. L.params . L.command
          args = fromMaybe [] (req ^. L.params . L.arguments)
      result <- liftIO $ try (executeCommand env cmd args)
      respond $ case result of
        Left (e :: SomeException) -> Left (TResponseError (InR ErrorCodes_InvalidParams) (T.pack (show e)) Nothing)
        Right Nothing -> Right (InR Null)
        Right (Just v) -> Right (InL v)
  ]

-- An edit to a task's entry or to one of its dependencies means a render is due
documentTouched :: ServerEnv -> Uri -> Maybe Int32 -> IO ()
documentTouched env uri version = case uriToFilePath uri of
  Nothing -> return ()
  Just path -> do
    tasks <- M.elems <$> readTVarIO (envTasks env)
    forM_ tasks $ \task -> do
      deps <- readTVarIO (taskDeps task)
      when (path == taskEntryPath task) $
        forM_ version $ \v -> atomically (writeTVar (taskVersion task) (Just v))
      when (path == taskEntryPath task || path `S.member` deps) $
        atomically (writeTVar (taskDirty task) True)

executeCommand :: ServerEnv -> Text -> [Value] -> IO (Maybe Value)
executeCommand env cmd args
  | cmd == commandStart = case args of
      [v] | Success (params :: StartParams) <- fromJSON v -> Just . toJSON <$> startTask env params
      _ -> bad "expected [StartParams]"
  | cmd == commandStop = withTaskArg $ \task -> stopTask env task
  | cmd == commandRebuild = withTaskArg $ \task -> markDirty task
  | cmd == commandSetPaused = case args of
      [tid, pausedV] | Success (tid' :: Text) <- fromJSON tid, Success (paused :: Bool) <- fromJSON pausedV ->
        lookupTask tid' >>= mapM_ (\task -> atomically (writeTVar (taskPaused task) paused)) >> return Nothing
      _ -> bad "expected [taskId, bool]"
  | cmd == commandResync = withTaskArg $ \task -> do
      atomically (writeTVar (taskResync task) True)
      markDirty task
  | otherwise = bad [i|unknown command #{cmd}|]
  where
    bad :: Text -> IO a
    bad msg = ioError (userError (T.unpack msg))

    lookupTask tid = M.lookup tid <$> readTVarIO (envTasks env)

    withTaskArg f = case args of
      (tid : _) | Success (tid' :: Text) <- fromJSON tid -> do
        lookupTask tid' >>= mapM_ f
        return Nothing
      _ -> bad "expected [taskId]"

    markDirty task = atomically (writeTVar (taskDirty task) True)

startTask :: ServerEnv -> StartParams -> IO StartResult
startTask env (StartParams {..}) = do
  path <- case uriToFilePath (Uri startUri) of
    Just p -> return p
    Nothing -> ioError (userError [i|not a file uri: #{startUri}|])
  existing <- M.lookup startTaskId <$> readTVarIO (envTasks env)
  mapM_ (stopTask env) existing

  let workDir = envWorkRoot env </> T.unpack (T.map safeChar startTaskId)
  createDirectoryIfMissing True workDir
  task <- Task startTaskId path (toNormalizedUri (Uri startUri)) (M.union startOptions (envDefaultOptions env))
            (fromMaybe 100 startDebounceMs) workDir
            <$> newTVarIO True <*> newTVarIO False <*> newTVarIO False <*> newTVarIO (S.singleton path)
            <*> newTVarIO Nothing <*> newTVarIO Nothing <*> newTVarIO 1 <*> newTVarIO Nothing
  atomically $ modifyTVar' (envTasks env) (M.insert startTaskId task)
  worker <- async (runWorker env task)
  atomically $ writeTVar (taskWorker task) (Just worker)
  logLine [i|started task #{startTaskId} for #{path}|]
  return (StartResult startTaskId)
  where
    safeChar c | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] || c `elem` ("-_" :: String) = c
               | otherwise = '_'

stopTask :: ServerEnv -> Task -> IO ()
stopTask env task = do
  atomically $ modifyTVar' (envTasks env) (M.delete (taskId task))
  readTVarIO (taskWorker task) >>= mapM_ cancel
  doesDirectoryExist (taskWorkDir task) >>= \case
    True -> removeDirectoryRecursive (taskWorkDir task)
    False -> return ()
  logLine [i|stopped task #{taskId task}|]

-- Render whenever the task is dirty and unpaused: a quick pass once edits go quiet for the
-- debounce period, then a settling build once they go quiet for longer.
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
    False -> renderOnce env task False >>= \case
      False -> return ()
      True -> quietFor 1500 >>= \case
        False -> return ()
        True -> void (renderOnce env task True)
  where
    quietFor ms = race (threadDelay (ms * 1000)) (atomically (readTVar (taskDirty task) >>= check)) >>= \case
      Left () -> return True
      Right () -> return False

-- | One render of the current buffers. False when it was superseded by an edit or failed.
renderOnce :: ServerEnv -> Task -> Bool -> IO Bool
renderOnce env task settle = do
  (entryText, version, overlays) <- snapshotBuffers env task
  sendStatus env task StatusRendering Nothing Nothing []
  sendLog env task LogStdout "" True
  start <- getCurrentTime
  let request = RenderRequest {
        requestEntryPath = taskEntryPath task
        , requestEntryText = entryText
        , requestOverlays = overlays
        , requestWorkDir = taskWorkDir task
        , requestOptions = taskOptions task
        , requestSettle = settle
        , requestLog = \stream text -> sendLog env task stream text False
        }
  outcome <- race (atomically (readTVar (taskDirty task) >>= check)) (try (backendRender (envBackend env) request))
  end <- getCurrentTime
  let ms = realToFrac (diffUTCTime end start) * 1000 :: Double
  case outcome of
    Left () -> do
      logLine [i|render of #{taskId task} superseded by an edit|]
      return False
    Right (Left (e :: SomeException)) -> do
      let (code, msg) = case fromException e of
            Just (RenderFailure c m) -> (c, m)
            Nothing -> (-1, T.pack (show e))
      sendLog env task LogStderr msg False
      sendStatus env task StatusErrored (Just code) (Just ms) []
      return False
    Right (Right result) -> do
      publish env task result version ms
      sendStatus env task StatusReady Nothing (Just ms) (resultTimings result)
      return True

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

publish :: ServerEnv -> Task -> RenderResult -> Maybe Int32 -> Double -> IO ()
publish env task (RenderResult {..}) version ms = do
  resync <- atomically (swapTVar (taskResync task) False)
  lastOutput <- readTVarIO (taskLast task)
  encoded <- case lastOutput of
    Just (baseId, baseBytes) | not resync -> Xdelta.encode resultBytes baseBytes >>= \case
      Right patch
        | B.null patch -> return Nothing
        | otherwise -> return (Just (EncodingXdelta3, Just baseId, patch))
      Left err -> do
        logLine [i|xdelta3 failed, sending a full output: #{err}|]
        return (Just (EncodingFull, Nothing, resultBytes))
    _ -> return (Just (EncodingFull, Nothing, resultBytes))

  atomically $ writeTVar (taskDeps task) (S.fromList (taskEntryPath task : resultDeps))
  case encoded of
    Nothing -> return ()
    Just (encoding, baseId, bytes) -> do
      renderId <- atomically $ stateTVar (taskNextRenderId task) (\n -> (n, n + 1))
      atomically $ writeTVar (taskLast task) (Just (renderId, resultBytes))
      send env methodOutput $ OutputParams {
        outputTaskId = taskId task
        , outputRenderId = renderId
        , outputBaseRenderId = baseId
        , outputDocumentVersion = version
        , outputFiles = [OutputFile resultFileName encoding (T.decodeUtf8 (B64.encode bytes)) (B.length resultBytes)]
        , outputDeps = resultDeps
        , outputRenderMs = ms
        , outputTimings = resultTimings
        }

sendStatus :: ServerEnv -> Task -> Status -> Maybe Int -> Maybe Double -> [Timing] -> IO ()
sendStatus env task status code ms timings =
  send env methodStatus (StatusParams (taskId task) status code ms timings)

sendLog :: ServerEnv -> Task -> LogStream -> Text -> Bool -> IO ()
sendLog env task stream text clear = send env methodLog (LogParams (taskId task) stream text clear)

send :: A.ToJSON a => ServerEnv -> Text -> a -> IO ()
send env method params = readTVarIO (envLsp env) >>= \case
  Nothing -> return ()
  Just lspEnv -> runLspT lspEnv $ case method of
    "codedown/render/status" -> sendNotification (SMethod_CustomMethod (Proxy @"codedown/render/status")) (toJSON params)
    "codedown/render/output" -> sendNotification (SMethod_CustomMethod (Proxy @"codedown/render/output")) (toJSON params)
    "codedown/render/log" -> sendNotification (SMethod_CustomMethod (Proxy @"codedown/render/log")) (toJSON params)
    _ -> return ()

logLine :: Text -> IO ()
logLine = hPutStrLn stderr . T.unpack . ("render-server: " <>)
