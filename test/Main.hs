{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Applicative (Alternative)
import Control.Monad (forM_, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((&), (.~))
import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson (Value, decode, encode, toJSON)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import GHC.TypeLits (symbolVal)
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Render.Command (mapDeps)
import Render.Protocol
import qualified Render.Xdelta as Xdelta
import System.Directory
import System.FilePath
import System.Process (proc)
import Test.Sandwich
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)


spec :: TopSpec
spec = do
  describe "xdelta3" $ do
    it "round-trips a patch" $ do
      let old = B8.pack (concat (replicate 200 "the quick brown fox "))
      let new = B8.pack (concat (replicate 100 "the quick brown fox ") <> "jumped" <> concat (replicate 100 "the quick brown fox "))
      Right patch <- liftIO (Xdelta.encode new old)
      Right rebuilt <- liftIO (Xdelta.decode patch old)
      rebuilt `shouldBe` new
      (B8.length patch < B8.length new) `shouldBe` True

    it "gives an empty patch for equal inputs" $ do
      Right patch <- liftIO (Xdelta.encode "same" "same")
      patch `shouldBe` ""

  describe "protocol" $ do
    it "writes camelCase fields and lower-case tags" $ do
      let params = StatusParams "t1" StatusErrored PhaseQuick (Just "edit to x.tex (v3)") (Just 12) Nothing []
      decode (encode params) `shouldBe` (decode [i|{"taskId":"t1","status":"errored","phase":"quick","reason":"edit to x.tex (v3)","exitCode":12,"timings":[]}|] :: Maybe Value)

    it "reads start params" $ do
      decode [i|{"taskId":"a","uri":"file:///home/x.tex","format":"pdf","options":{"engine":"xelatex"},"debounceMs":50}|]
        `shouldBe` Just (StartParams "a" "file:///home/x.tex" "pdf" (M.fromList [("engine", "xelatex")]) (Just 50))

  describe "deps" $ do
    it "maps the work directory's copies back to the documents they stand for" $ do
      let reported = ["/nix/store/abc-texlive/article.cls", "/work/src/main.tex", "./sub/inc.tex", "/work/overlay/sub/other.tex", "/work/out/main.aux", "/home/u/other/img.png"]
      mapDeps "/home/u/docs" "/home/u/docs/main.tex" "/work" reported
        `shouldBe` ["/home/u/docs/main.tex", "/home/u/docs/sub/inc.tex", "/home/u/docs/sub/other.tex", "/home/u/other/img.png"]

  describe "server" $ do
    it "renders on start and sends the full output with what it read from disk" $ do
      (docDir, script) <- setup
      (_, _, out) <- withServer docDir ["--render", script] $ do
        void $ start docDir 50 mempty
        untilOutput

      outputRenderId out `shouldBe` 1
      outputBaseRenderId out `shouldBe` Nothing
      outputDeps out `shouldBe` [docDir </> "inc.txt"]
      fullBytes out `shouldBe` "hello\ninc1\nquick\n"

    it "answers an edit with a diff against the last render, tagged with the document version" $ do
      (docDir, script) <- setup
      (first, second) <- withServer docDir ["--render", script] $ do
        doc <- start docDir 50 mempty
        (_, _, first) <- untilOutput
        changeDoc doc [whole "bye\n"]
        (_, _, second) <- untilOutput
        return (first, second)

      outputBaseRenderId second `shouldBe` Just 1
      outputDocumentVersion second `shouldBe` Just 1
      Right rebuilt <- liftIO (Xdelta.decode (patchBytes second) (fullBytes first))
      rebuilt `shouldBe` "bye\ninc1\nquick\n"

    it "coalesces a burst of edits into one render" $ do
      (docDir, script) <- setup
      (statuses, out) <- withServer docDir ["--render", script] $ do
        doc <- start docDir 300 mempty
        void untilOutput
        forM_ [1 .. 5 :: Int] $ \n -> do
          changeDoc doc [whole [i|edit #{n}\n|]]
          threadDelay 20_000
        (statuses, _, out) <- untilOutput
        return (statuses, out)

      length [s | s <- statuses, statusStatus s == StatusRendering] `shouldBe` 1
      outputDocumentVersion out `shouldBe` Just 5

    it "runs the settle program once edits stay quiet" $ do
      (docDir, script) <- setup
      (first, second, settled) <- withServer docDir ["--render", script, "--settle", script] $ do
        void $ start docDir 50 mempty
        (_, _, first) <- untilOutput
        (_, _, second) <- untilOutput
        (_, settled) <- untilStatus ((== PhaseSettle) . statusPhase)
        return (first, second, settled)

      fullBytes first `shouldBe` "hello\ninc1\nquick\n"
      Right bytes <- liftIO (Xdelta.decode (patchBytes second) (fullBytes first))
      bytes `shouldBe` "hello\ninc1\nsettle\n"
      statusStatus settled `shouldBe` StatusReady

    it "has no settle pass without a settle program" $ do
      (docDir, script) <- setup
      statuses <- withServer docDir ["--render", script] $ do
        doc <- start docDir 50 mempty
        void untilOutput
        threadDelay 2_500_000
        changeDoc doc [whole "bye\n"]
        (statuses, _, _) <- untilOutput
        return statuses

      [statusPhase s | s <- statuses] `shouldBe` [PhaseQuick, PhaseQuick]

    it "rebuilds only when a dependency changed on disk" $ do
      (docDir, script) <- setup
      (statuses, first, second) <- withServer docDir ["--render", script] $ do
        void $ start docDir 50 mempty
        (_, _, first) <- untilOutput
        execute commandRebuild [toJSON ("t1" :: Text)]
        liftIO $ writeFile (docDir </> "inc.txt") "inc2\n"
        execute commandRebuild [toJSON ("t1" :: Text)]
        (statuses, _, second) <- untilOutput
        return (statuses, first, second)

      [statusReason s | s <- statuses, statusStatus s == StatusRendering] `shouldBe` [Just "inc.txt changed on disk"]
      Right rebuilt <- liftIO (Xdelta.decode (patchBytes second) (fullBytes first))
      rebuilt `shouldBe` "hello\ninc2\nquick\n"

    it "lets an open buffer shadow its disk copy and re-renders on its edits" $ do
      (docDir, script) <- setup
      (first, second) <- withServer docDir ["--render", script] $ do
        inc <- openDoc "inc.txt" "plaintext"
        void $ start docDir 50 mempty
        (_, _, first) <- untilOutput
        changeDoc inc [whole "inc-edited\n"]
        (_, _, second) <- untilOutput
        return (first, second)

      fullBytes first `shouldBe` "hello\ninc1\nquick\n"
      outputDeps first `shouldBe` []
      Right rebuilt <- liftIO (Xdelta.decode (patchBytes second) (fullBytes first))
      rebuilt `shouldBe` "hello\ninc-edited\nquick\n"

    it "passes the task's options and the command line's defaults as RENDER_OPT variables" $ do
      (docDir, script) <- setup
      (_, _, out) <- withServer docDir ["--render", script, "-o", "flavor=cli", "-o", "size=big"] $ do
        void $ start docDir 50 (M.fromList [("flavor", "vanilla")])
        untilOutput

      fullBytes out `shouldBe` "hello\ninc1\nquick\nflavor=vanilla size=big\n"

    it "reports a failing program with its exit code and stderr" $ do
      (docDir, _) <- setup
      failing <- writeScript docDir "fail.sh" "#!/bin/sh\necho boom >&2\nexit 3\n"
      (logs, status) <- withServer docDir ["--render", failing] $ do
        void $ start docDir 50 mempty
        untilStatus ((== StatusErrored) . statusStatus)

      statusExitCode status `shouldBe` Just 3
      [logText l | l <- logs, logStream l == LogStderr, "boom" `T.isInfixOf` logText l] `shouldBe` ["boom"]

    it "holds renders while paused" $ do
      (docDir, script) <- setup
      (statuses, out) <- withServer docDir ["--render", script] $ do
        doc <- start docDir 50 mempty
        void untilOutput
        execute commandSetPaused [toJSON ("t1" :: Text), toJSON True]
        changeDoc doc [whole "one\n"]
        threadDelay 500_000
        changeDoc doc [whole "two\n"]
        execute commandSetPaused [toJSON ("t1" :: Text), toJSON False]
        (statuses, _, out) <- untilOutput
        return (statuses, out)

      length [s | s <- statuses, statusStatus s == StatusRendering] `shouldBe` 1
      outputDocumentVersion out `shouldBe` Just 2

    it "sends the full output again on resync" $ do
      (docDir, script) <- setup
      (first, second) <- withServer docDir ["--render", script] $ do
        void $ start docDir 50 mempty
        (_, _, first) <- untilOutput
        execute commandResync [toJSON ("t1" :: Text)]
        (_, _, second) <- untilOutput
        return (first, second)

      outputBaseRenderId second `shouldBe` Nothing
      outputRenderId second `shouldBe` 2
      fullBytes second `shouldBe` fullBytes first

-- The document directory with main.txt and inc.txt, and a render program that concatenates them
-- (the open buffer of inc.txt if there is one), appends the phase, and reports what it read
setup :: HasBaseContext context => ExampleT context IO (FilePath, FilePath)
setup = do
  Just dir <- getCurrentFolder
  let docDir = dir </> "doc"
  liftIO $ createDirectoryIfMissing True docDir
  liftIO $ writeFile (docDir </> "main.txt") "hello\n"
  liftIO $ writeFile (docDir </> "inc.txt") "inc1\n"
  script <- writeScript dir "render.sh" [__i|\#!/bin/sh
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

-- The client does not offer workspace/configuration: lsp-test answers that request from its
-- listener thread while the session thread is still writing, and the two writes interleave on the
-- server's stdin
withServer :: FilePath -> [String] -> Session (ExampleT context IO) a -> ExampleT context IO a
withServer root args = runSessionWithConfig defaultConfig { messageTimeout = 20 } (proc "lsp-export-server" ("serve" : args)) caps root
  where
    caps = fullLatestClientCaps & L.workspace . traverse . L.configuration .~ Just False

-- Opens main.txt and starts task t1 on it
start :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m) => FilePath -> Int -> M.Map Text Text -> Session m TextDocumentIdentifier
start docDir debounceMs options = do
  doc <- openDoc "main.txt" "plaintext"
  execute commandStart [toJSON (StartParams "t1" (T.pack (filePathToUri' (docDir </> "main.txt"))) "txt" options (Just debounceMs))]
  return doc
  where
    filePathToUri' = T.unpack . getUri . filePathToUri

execute :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m) => Text -> [Value] -> Session m ()
execute cmd args = do
  TResponseMessage {_result} <- request SMethod_WorkspaceExecuteCommand (ExecuteCommandParams Nothing cmd (Just args))
  case _result of
    Left err -> liftIO $ expectationFailure [i|#{cmd} failed: #{err}|]
    Right _ -> return ()

whole :: Text -> TextDocumentContentChangeEvent
whole text = TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument text))

data Event = EvStatus StatusParams | EvOutput OutputParams | EvLog LogParams | EvOther

classify :: FromServerMessage -> Event
classify = \case
  FromServerMess m msg -> case splitServerMethod m of
    IsServerEither -> case msg of
      NotMess (TNotificationMessage _ (SMethod_CustomMethod p) params) -> byName (T.pack (symbolVal p)) params
      _ -> EvOther
    _ -> EvOther
  _ -> EvOther
  where
    byName name params
      | name == methodStatus = parsed EvStatus params
      | name == methodOutput = parsed EvOutput params
      | name == methodLog = parsed EvLog params
      | otherwise = EvOther

    parsed f params = case A.fromJSON params of
      A.Success x -> f x
      A.Error _ -> EvOther

-- | The statuses and log lines up to the next output, and the output
untilOutput :: (MonadLoggerIO m, MonadUnliftIO m) => Session m ([StatusParams], [LogParams], OutputParams)
untilOutput = go [] []
  where
    go statuses logs = anyMessage >>= \msg -> case classify msg of
      EvOutput o -> return (reverse statuses, reverse logs, o)
      EvStatus s | statusStatus s == StatusErrored -> liftIO $ expectationFailure [i|render failed (exit #{statusExitCode s}); log: #{[logText l | l <- reverse logs]}|]
      EvStatus s -> go (s : statuses) logs
      EvLog l -> go statuses (l : logs)
      EvOther -> go statuses logs

-- | The log lines up to the first status matching the predicate, and that status
untilStatus :: (MonadLoggerIO m, MonadUnliftIO m) => (StatusParams -> Bool) -> Session m ([LogParams], StatusParams)
untilStatus p = go []
  where
    go logs = anyMessage >>= \msg -> case classify msg of
      EvStatus s | p s -> return (reverse logs, s)
      EvLog l -> go (l : logs)
      _ -> go logs

fileBytes :: OutputParams -> ByteString
fileBytes (OutputParams {outputFiles = [f]}) = B64.decodeLenient (T.encodeUtf8 (outputFileBytes f))
fileBytes _ = error "expected exactly one output file"

fullBytes :: OutputParams -> ByteString
fullBytes out@(OutputParams {outputFiles = [OutputFile {outputFileEncoding = EncodingFull}]}) = fileBytes out
fullBytes _ = error "expected a full output"

patchBytes :: OutputParams -> ByteString
patchBytes out@(OutputParams {outputFiles = [OutputFile {outputFileEncoding = EncodingXdelta3}]}) = fileBytes out
patchBytes _ = error "expected an xdelta3 output"

main :: IO ()
main = runSandwichWithCommandLineArgs options spec
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (formatTime defaultTimeLocale "%Y-%m-%dT%H.%M.%S" <$> getCurrentTime)
      }
