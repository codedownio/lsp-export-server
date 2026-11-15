{-# LANGUAGE TypeOperators #-}

module Render.Server.Env (
  ServerEnv(..)
  , Task(..)
  , markDirty
  , taskFormat
  , sendStatus
  , sendLog
  , send
  , logLine
  ) where

import Data.Aeson (toJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Render.Command
import Render.Protocol
import System.IO (hPutStrLn, stderr)
import UnliftIO (Async)
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
  , taskReasons :: TVar [Text]
  , taskEdit :: TVar (Maybe (FilePath, Position))
  , taskRenderedEdit :: TVar (Maybe (FilePath, Position))
  , taskPaused :: TVar Bool
  , taskResync :: TVar Bool
  , taskDeps :: TVar (Set FilePath)
  , taskDepHashes :: TVar (Map FilePath (Maybe B.ByteString))
  , taskVersion :: TVar (Maybe Int32)
  , taskLast :: TVar (Maybe (Int, B.ByteString))
  , taskNextRenderId :: TVar Int
  , taskWorker :: TVar (Maybe (Async ()))
  }

data ServerEnv = ServerEnv {
  envCommand :: Programs
  , envWorkRoot :: FilePath
  , envDefaultOptions :: Map Text Text
  , envTasks :: TVar (Map Text Task)
  , envLsp :: TVar (Maybe (LanguageContextEnv ()))
  }

markDirty :: Task -> Text -> IO ()
markDirty task reason = atomically $ do
  writeTVar (taskDirty task) True
  modifyTVar' (taskReasons task) (<> [reason])

taskFormat :: Task -> Text
taskFormat task = fromMaybe "" (M.lookup "format" (taskOptions task))

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
