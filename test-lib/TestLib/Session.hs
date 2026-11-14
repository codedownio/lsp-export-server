module TestLib.Session (
  setupDocument
  , writeScript
  , withServer
  , start
  , execute
  , executeResult
  , whole
  , insertAt
  ) where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Render.Protocol
import System.Directory
import System.FilePath
import System.Process (proc)
import Test.Sandwich


-- | A document directory with main.txt and inc.txt, and a render program that concatenates them
-- (the open buffer of inc.txt if there is one), appends the phase, and reports what it read.
setupDocument :: HasBaseContext context => ExampleT context IO (FilePath, FilePath)
setupDocument = do
  Just dir <- getCurrentFolder
  let docDir = dir </> "doc"
  liftIO $ createDirectoryIfMissing True docDir
  liftIO $ writeFile (docDir </> "main.txt") "hello\n"
  liftIO $ writeFile (docDir </> "inc.txt") "inc1\n"
  script <- writeScript dir "render.sh" [__i|
    \#!/bin/sh
    set -e
    echo "rendering $RENDER_PHASE of $RENDER_JOB.$RENDER_FORMAT"
    inc=inc.txt
    [ -f "$RENDER_OVERLAY/inc.txt" ] && inc="$RENDER_OVERLAY/inc.txt"
    cat "$RENDER_ENTRY" "$inc" > "$RENDER_OUTPUT"
    echo "$RENDER_PHASE" >> "$RENDER_OUTPUT"
    if [ -n "${RENDER_OPT_flavor:-}" ]; then echo "flavor=$RENDER_OPT_flavor size=${RENDER_OPT_size:-}" >> "$RENDER_OUTPUT"; fi
    printf '%s\\n' "$RENDER_ENTRY" "$inc" /nix/store/abc-lib/x > "$RENDER_DEPS"
  |]
  return (docDir, script)

writeScript :: FilePath -> FilePath -> String -> ExampleT context IO FilePath
writeScript dir name body = liftIO $ do
  let path = dir </> name
  writeFile path body
  getPermissions path >>= setPermissions path . setOwnerExecutable True
  return path

-- | Runs the built server on the given document directory. The client does not offer
-- workspace/configuration: lsp-test answers that request from its listener thread while the session
-- thread is still writing, and the two writes interleave on the server's stdin.
withServer :: FilePath -> [String] -> Session (ExampleT context IO) a -> ExampleT context IO a
withServer root args = runSessionWithConfig config (proc "lsp-export-server" ("serve" : args)) caps root
  where
    config = defaultConfig { messageTimeout = 20 }
    caps = fullLatestClientCaps & L.workspace . traverse . L.configuration .~ Just False

-- | Opens main.txt and starts task t1 on it.
start :: FilePath -> Int -> M.Map Text Text -> Session (ExampleT context IO) TextDocumentIdentifier
start docDir debounceMs options = do
  doc <- openDoc "main.txt" "plaintext"
  let uri = T.unpack (getUri (filePathToUri (docDir </> "main.txt")))
  execute commandStart [toJSON (StartParams "t1" (T.pack uri) "txt" options (Just debounceMs))]
  return doc

execute :: Text -> [Value] -> Session (ExampleT context IO) ()
execute cmd args = void (executeResult cmd args)

executeResult :: Text -> [Value] -> Session (ExampleT context IO) Value
executeResult cmd args = do
  TResponseMessage {_result} <- request SMethod_WorkspaceExecuteCommand (ExecuteCommandParams Nothing cmd (Just args))
  case _result of
    Left err -> liftIO $ expectationFailure [i|#{cmd} failed: #{err}|]
    Right (InL v) -> return v
    Right (InR _) -> return A.Null

whole :: Text -> TextDocumentContentChangeEvent
whole text = TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument text))

-- | Insert text at a zero-based line and character
insertAt :: UInt -> UInt -> Text -> TextDocumentContentChangeEvent
insertAt line character text = TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range pos pos) Nothing text))
  where pos = Position line character
