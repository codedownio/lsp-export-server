module Spec.Server (tests, main) where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Test
import Render.Protocol
import qualified Render.Xdelta as Xdelta
import System.FilePath
import Test.Sandwich
import TestLib.Events
import TestLib.Main
import TestLib.Session
import UnliftIO.Concurrent (threadDelay)


tests :: HasBaseContext context => SpecFree context IO ()
tests = describe "server" $ do
  it "renders on start and sends the full output with what it read from disk" $ do
    (docDir, script) <- setupDocument
    (_, _, out) <- withServer docDir ["--render", script] $ do
      void $ start docDir 50 mempty
      untilOutput

    outputRenderId out `shouldBe` 1
    outputBaseRenderId out `shouldBe` Nothing
    outputDeps out `shouldBe` [docDir </> "inc.txt"]
    fullBytes out `shouldBe` "hello\ninc1\nquick\n"

  it "answers an edit with a diff against the last render, tagged with the document version" $ do
    (docDir, script) <- setupDocument
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
    (docDir, script) <- setupDocument
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
    (docDir, script) <- setupDocument
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
    (docDir, script) <- setupDocument
    statuses <- withServer docDir ["--render", script] $ do
      doc <- start docDir 50 mempty
      void untilOutput
      threadDelay 2_500_000
      changeDoc doc [whole "bye\n"]
      (statuses, _, _) <- untilOutput
      return statuses

    [statusPhase s | s <- statuses] `shouldBe` [PhaseQuick, PhaseQuick]

  it "rebuilds only when a dependency changed on disk" $ do
    (docDir, script) <- setupDocument
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
    (docDir, script) <- setupDocument
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
    (docDir, script) <- setupDocument
    (_, _, out) <- withServer docDir ["--render", script, "-o", "flavor=cli", "-o", "size=big"] $ do
      void $ start docDir 50 (M.fromList [("flavor", "vanilla")])
      untilOutput

    fullBytes out `shouldBe` "hello\ninc1\nquick\nflavor=vanilla size=big\n"

  it "reports a failing program with its exit code and stderr" $ do
    (docDir, _) <- setupDocument
    failing <- writeScript docDir "fail.sh" [__i|
      \#!/bin/sh
      echo boom >&2
      exit 3
    |]
    (logs, status) <- withServer docDir ["--render", failing] $ do
      void $ start docDir 50 mempty
      untilStatus ((== StatusErrored) . statusStatus)

    statusExitCode status `shouldBe` Just 3
    [logText l | l <- logs, logStream l == LogStderr, "boom" `T.isInfixOf` logText l] `shouldBe` ["boom"]

  it "holds renders while paused" $ do
    (docDir, script) <- setupDocument
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
    (docDir, script) <- setupDocument
    (first, second) <- withServer docDir ["--render", script] $ do
      void $ start docDir 50 mempty
      (_, _, first) <- untilOutput
      execute commandResync [toJSON ("t1" :: Text)]
      (_, _, second) <- untilOutput
      return (first, second)

    outputBaseRenderId second `shouldBe` Nothing
    outputRenderId second `shouldBe` 2
    fullBytes second `shouldBe` fullBytes first

main :: IO ()
main = specMain tests
