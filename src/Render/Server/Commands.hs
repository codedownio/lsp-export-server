module Render.Server.Commands (
  executeCommand
  ) where

import Control.Monad
import Data.Aeson (Result(..), Value, fromJSON, toJSON)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Language.LSP.Protocol.Types
import Render.Command
import Render.Protocol
import Render.Server.Env
import Render.Server.Publish
import Render.Server.Worker
import System.Directory
import System.FilePath
import UnliftIO (async, cancel)
import UnliftIO.STM


executeCommand :: ServerEnv -> Text -> [Value] -> IO (Maybe Value)
executeCommand env cmd args
  | cmd == commandStart = case args of
      [v] | Success (params :: StartParams) <- fromJSON v -> Just . toJSON <$> startTask env params
      _ -> bad "expected [StartParams]"
  | cmd == commandStop = withTaskArg $ \task -> stopTask env task
  | cmd == commandRebuild = withTaskArg $ \task -> do
      known <- readTVarIO (taskDepHashes task)
      changed <- filterM (\(path, h) -> (/= h) <$> hashFile path) (M.toList known)
      case changed of
        [] -> logLine [i|rebuild of #{taskId task} requested, but no known dependency changed on disk|]
        _ -> markDirty task [i|#{T.intercalate ", " [T.pack (takeFileName p) | (p, _) <- changed]} changed on disk|]
  | cmd == commandSetPaused = case args of
      [tid, pausedV] | Success (tid' :: Text) <- fromJSON tid, Success (paused :: Bool) <- fromJSON pausedV ->
        lookupTask tid' >>= mapM_ (\task -> atomically (writeTVar (taskPaused task) paused)) >> return Nothing
      _ -> bad "expected [taskId, bool]"
  | cmd == commandResync = withTaskArg $ \task -> do
      atomically (writeTVar (taskResync task) True)
      markDirty task "full output requested by the client"
  | cmd == commandLocate = case args of
      [v] | Success (LocateParams {..}) <- fromJSON v -> lookupTask locateTaskId >>= \case
        Nothing -> return Nothing
        Just task -> case uriToFilePath (Uri locateUri) of
          Nothing -> bad [i|not a file uri: #{locateUri}|]
          Just path -> locatePosition env task path (Position (fromIntegral locateLine) (fromIntegral locateCharacter)) >>= \case
            Left err -> bad err
            Right box -> return (toJSON <$> box)
      _ -> bad "expected [LocateParams]"
  | cmd == commandSourceAt = case args of
      [v] | Success (SourceAtParams {..}) <- fromJSON v -> lookupTask sourceAtTaskId >>= \case
        Nothing -> return Nothing
        Just task -> case programLocate (envCommand env) of
          Nothing -> return Nothing
          Just exe -> sourceAt exe (taskEntryPath task) (taskWorkDir task) (taskFormat task) sourceAtPage sourceAtX sourceAtY >>= \case
            Left err -> bad err
            Right found -> return $ flip fmap found $ \(path, line, column) ->
              toJSON (SourceLocation (getUri (filePathToUri path)) (max 0 (line - 1)) (max 0 (column - 1)))
      _ -> bad "expected [SourceAtParams]"
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
            <$> newTVarIO True <*> newTVarIO ["task started"] <*> newTVarIO Nothing <*> newTVarIO Nothing <*> newTVarIO False <*> newTVarIO False <*> newTVarIO (S.singleton path) <*> newTVarIO mempty
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
