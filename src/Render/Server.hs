{-# LANGUAGE TypeOperators #-}

module Render.Server (
  runRenderServer
  ) where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (check)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (Result(..), Value, fromJSON, toJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
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
  , taskEpoch :: Integer
  , taskWorkDir :: FilePath
  , taskDirty :: TVar Bool
  -- | What has made the task dirty since the last render started
  , taskReasons :: TVar [Text]
  , taskPaused :: TVar Bool
  , taskResync :: TVar Bool
  , taskDeps :: TVar (Set FilePath)
  -- | Content hashes of the on-disk dependencies as of the last render
  , taskDepHashes :: TVar (Map FilePath (Maybe B.ByteString))
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
        markDirty task [i|edit to #{takeFileName path}#{maybe "" (\v -> " (v" <> show v <> ")") version}|]

executeCommand :: ServerEnv -> Text -> [Value] -> IO (Maybe Value)
executeCommand env cmd args
  | cmd == commandStart = case args of
      [v] | Success (params :: StartParams) <- fromJSON v -> Just . toJSON <$> startTask env params
      _ -> bad "expected [StartParams]"
  | cmd == commandStop = withTaskArg $ \task -> stopTask env task
  | cmd == commandRebuild = withTaskArg $ \task -> do
      -- A rebuild is only worth doing when some dependency's content actually changed
      known <- readTVarIO (taskDepHashes task)
      changed <- filterM (\(path, h) -> (/= h) <$> hashFile path) (M.toList known)
      case (M.null known, changed) of
        (True, _) -> markDirty task "rebuild requested by the client"
        (False, []) -> logLine [i|rebuild of #{taskId task} requested, but no dependency changed on disk|]
        (False, _) -> markDirty task [i|#{T.intercalate ", " [T.pack (takeFileName p) | (p, _) <- changed]} changed on disk|]
  | cmd == commandSetPaused = case args of
      [tid, pausedV] | Success (tid' :: Text) <- fromJSON tid, Success (paused :: Bool) <- fromJSON pausedV ->
        lookupTask tid' >>= mapM_ (\task -> atomically (writeTVar (taskPaused task) paused)) >> return Nothing
      _ -> bad "expected [taskId, bool]"
  | cmd == commandResync = withTaskArg $ \task -> do
      atomically (writeTVar (taskResync task) True)
      markDirty task "full output requested by the client"
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

markDirty :: Task -> Text -> IO ()
markDirty task reason = atomically $ do
  writeTVar (taskDirty task) True
  modifyTVar' (taskReasons task) (<> [reason])

startTask :: ServerEnv -> StartParams -> IO StartResult
startTask env (StartParams {..}) = do
  path <- case uriToFilePath (Uri startUri) of
    Just p -> return p
    Nothing -> ioError (userError [i|not a file uri: #{startUri}|])
  existing <- M.lookup startTaskId <$> readTVarIO (envTasks env)
  mapM_ (stopTask env) existing

  let workDir = envWorkRoot env </> T.unpack (T.map safeChar startTaskId)
  createDirectoryIfMissing True workDir
  epoch <- round <$> getPOSIXTime
  let options = M.insert "format" startFormat (M.union startOptions (envDefaultOptions env))
  task <- Task startTaskId path (toNormalizedUri (Uri startUri)) options
            (fromMaybe 100 startDebounceMs) epoch workDir
            <$> newTVarIO True <*> newTVarIO ["task started"] <*> newTVarIO False <*> newTVarIO False <*> newTVarIO (S.singleton path) <*> newTVarIO mempty
            <*> newTVarIO Nothing <*> newTVarIO Nothing <*> newTVarIO 1 <*> newTVarIO Nothing
  atomically $ modifyTVar' (envTasks env) (M.insert startTaskId task)
  worker <- async (runWorker env task)
  atomically $ writeTVar (taskWorker task) (Just worker)
  logLine [i|started task #{startTaskId} for #{path} (debounce #{taskDebounceMs task}ms)|]
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
    False -> renderOnce env task PhaseQuick >>= \case
      False -> return ()
      True -> quietFor settleQuietMs >>= \case
        False -> return ()
        True -> void (renderOnce env task PhaseSettle)
  where
    quietFor ms = race (threadDelay (ms * 1000)) (atomically (readTVar (taskDirty task) >>= check)) >>= \case
      Left () -> return True
      Right () -> return False

settleQuietMs :: Int
settleQuietMs = 1500

-- | One render of the current buffers. False when it was superseded by an edit or failed.
renderOnce :: ServerEnv -> Task -> Phase -> IO Bool
renderOnce env task phase = do
  (entryText, version, overlays) <- snapshotBuffers env task
  reasons <- atomically (swapTVar (taskReasons task) [])
  let reason = case reasons of
        [] -> Nothing
        _ -> Just (T.intercalate ", " reasons)
      settle = phase == PhaseSettle
      label = case phase of
        PhaseQuick -> "quick render" :: Text
        PhaseSettle -> "settle pass"
  -- The settle pass stays out of the log unless it fails or changes the output
  buffered <- newTVarIO ([] :: [(LogStream, Text)])
  let logFn stream text = case phase of
        PhaseQuick -> sendLog env task stream text False
        PhaseSettle -> atomically (modifyTVar' buffered ((stream, text) :))
      marker text = logFn LogStdout ("── " <> text)
      flush = do
        lines' <- reverse <$> atomically (swapTVar buffered [])
        forM_ lines' $ \(stream, text) -> sendLog env task stream text False
  case phase of
    PhaseQuick -> do
      sendStatus env task StatusRendering phase reason Nothing Nothing []
      sendLog env task LogStdout "" True
      marker [i|#{label}: #{fromMaybe "no reason recorded" reason}|]
    PhaseSettle -> marker [i|#{label} (latexmk) after #{settleQuietMs}ms without edits|]
  start <- getCurrentTime
  let request = RenderRequest {
        requestEntryPath = taskEntryPath task
        , requestEntryText = entryText
        , requestOverlays = overlays
        , requestWorkDir = taskWorkDir task
        , requestOptions = taskOptions task
        , requestSettle = settle
        , requestEpoch = taskEpoch task
        , requestLog = logFn
        }
  outcome <- race (atomically (readTVar (taskDirty task) >>= check)) (try (backendRender (envBackend env) request))
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
      sent <- publish env task result (map fst overlays) version ms
      marker [i|#{label} done in #{showMs ms}: #{fromMaybe "output unchanged, nothing sent" sent}|]
      when (isJust sent) flush
      sendStatus env task StatusReady phase reason Nothing (Just ms) (resultTimings result)
      return True

hashFile :: FilePath -> IO (Maybe B.ByteString)
hashFile path = doesFileExist path >>= \case
  False -> return Nothing
  True -> Just . SHA256.hash <$> B.readFile path

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

-- | Send the output, as a diff against the last one where possible, and say what was sent (Nothing
-- when it was identical to the last one). The deps a client should watch on disk leave out the
-- buffers it sends us itself.
publish :: ServerEnv -> Task -> RenderResult -> [FilePath] -> Maybe Int32 -> Double -> IO (Maybe Text)
publish env task (RenderResult {..}) overlayPaths version ms = do
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
        }
      return $ Just $ case encoding of
        EncodingFull -> [i|sent the full #{B.length bytes}B output as render #{renderId}|]
        EncodingXdelta3 -> [i|output changed, sent a #{B.length bytes}B diff as render #{renderId}|]

sendStatus :: ServerEnv -> Task -> Status -> Phase -> Maybe Text -> Maybe Int -> Maybe Double -> [Timing] -> IO ()
sendStatus env task status phase reason code ms timings =
  send env methodStatus (StatusParams (taskId task) status phase reason code ms timings)

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
logLine = hPutStrLn stderr . T.unpack . ("lsp-export-server: " <>)
