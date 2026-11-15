module Render.Server (
  runRenderServer
  ) where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Render.Command
import Render.Protocol
import Render.Server.Commands
import Render.Server.Env
import System.FilePath
import UnliftIO (SomeException, try)
import UnliftIO.STM


runRenderServer :: Programs -> FilePath -> Map Text Text -> IO Int
runRenderServer command workRoot defaultOpts = do
  env <- ServerEnv command workRoot defaultOpts <$> newTVarIO mempty <*> newTVarIO Nothing
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
      liftIO $ documentTouched env (msg ^. L.params . L.textDocument . L.uri) Nothing Nothing
  , notificationHandler SMethod_TextDocumentDidChange $ \msg ->
      liftIO $ documentTouched env (msg ^. L.params . L.textDocument . L.uri) (Just (msg ^. L.params . L.textDocument . L.version))
                                (lastEditPosition (msg ^. L.params . L.contentChanges))
  , notificationHandler SMethod_TextDocumentDidClose $ \_ -> return ()
  , notificationHandler SMethod_TextDocumentDidSave $ \_ -> return ()
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> return ()
  , notificationHandler SMethod_CancelRequest $ \_ -> return ()
  -- lsp always advertises semantic tokens
  , requestHandler SMethod_TextDocumentSemanticTokensFull $ \_ respond -> respond (Right (InR Null))
  , requestHandler SMethod_WorkspaceExecuteCommand $ \req respond -> do
      let args = fromMaybe [] (req ^. L.params . L.arguments)
      result <- liftIO $ try (executeCommand env (req ^. L.params . L.command) args)
      respond $ case result of
        Left (e :: SomeException) -> Left (TResponseError (InR ErrorCodes_InvalidParams) (T.pack (show e)) Nothing)
        Right Nothing -> Right (InR Null)
        Right (Just v) -> Right (InL v)
  ]

documentTouched :: ServerEnv -> Uri -> Maybe Int32 -> Maybe Position -> IO ()
documentTouched env uri version position = case uriToFilePath uri of
  Nothing -> return ()
  Just path -> do
    tasks <- M.elems <$> readTVarIO (envTasks env)
    forM_ tasks $ \task -> do
      deps <- readTVarIO (taskDeps task)
      when (path == taskEntryPath task) $
        forM_ version $ \v -> atomically (writeTVar (taskVersion task) (Just v))
      when (path == taskEntryPath task || path `S.member` deps) $ do
        forM_ position $ \pos -> atomically (writeTVar (taskEdit task) (Just (path, pos)))
        markDirty task [i|edit to #{takeFileName path}#{maybe "" (\v -> " (v" <> show v <> ")") version}|]

lastEditPosition :: [TextDocumentContentChangeEvent] -> Maybe Position
lastEditPosition changes = listToMaybe [ partial ^. L.range . L.start | TextDocumentContentChangeEvent (InL partial) <- reverse changes ]
