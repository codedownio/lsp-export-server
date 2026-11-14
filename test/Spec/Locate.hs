module Spec.Locate (tests, main) where

import Control.Monad (void)
import Data.Aeson (fromJSON, toJSON)
import qualified Data.Aeson as A
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types (filePathToUri, getUri)
import Language.LSP.Test
import Render.Protocol
import System.FilePath
import Test.Sandwich
import TestLib.Events
import TestLib.Main
import TestLib.Session


tests :: HasBaseContext context => SpecFree context IO ()
tests = describe "locate" $ do
  it "puts where the edit landed on the output that answers it" $ do
    (docDir, script) <- setupDocument
    locator <- locateScript
    (first, second) <- withServer docDir ["--render", script, "--locate", locator] $ do
      doc <- start docDir 50 mempty
      (_, _, first) <- untilOutput
      changeDoc doc [insertAt 0 4 "!"]
      (_, _, second) <- untilOutput
      return (first, second)

    outputPosition first `shouldBe` Nothing
    outputPosition second `shouldBe` Just (PageBox 1 1 5 100 10)

  it "sends no position without a locate program" $ do
    (docDir, script) <- setupDocument
    out <- withServer docDir ["--render", script] $ do
      doc <- start docDir 50 mempty
      void untilOutput
      changeDoc doc [insertAt 0 4 "!"]
      (_, _, out) <- untilOutput
      return out

    outputPosition out `shouldBe` Nothing

  it "answers locate and sourceAt from the last render, through the work directory's paths" $ do
    (docDir, script) <- setupDocument
    locator <- locateScript
    (inEntry, inOverlay, inDisk, source) <- withServer docDir ["--render", script, "--locate", locator] $ do
      void $ openDoc "inc.txt" "plaintext"
      void $ start docDir 50 mempty
      void untilOutput
      let locateIn name = executeResult commandLocate [toJSON (LocateParams "t1" (uriOf (docDir </> name)) 2 0)]
      inEntry <- locateIn "main.txt"
      inOverlay <- locateIn "inc.txt"
      inDisk <- locateIn "other.txt"
      source <- executeResult commandSourceAt [toJSON (SourceAtParams "t1" 3 10 20)]
      return (inEntry, inOverlay, inDisk, source)

    (fromJSON inEntry, fromJSON inOverlay, fromJSON inDisk) `shouldBe` (A.Success (PageBox 3 1 1 100 10), A.Success (PageBox 3 2 1 100 10), A.Success (PageBox 3 3 1 100 10))
    fromJSON source `shouldBe` A.Success (SourceLocation (uriOf (docDir </> "inc.txt")) 2 6)

main :: IO ()
main = specMain tests

-- | Forward: the line as the page, which of the work directory's copies the source is as the left
-- edge, the column as the top. Inverse: line page, column 7, in the open buffer of inc.txt.
locateScript :: HasBaseContext context => ExampleT context IO FilePath
locateScript = do
  Just dir <- getCurrentFolder
  writeScript dir "locate.sh" [__i|
    \#!/bin/sh
    set -e
    [ -f "$RENDER_OUTPUT" ]
    case "$RENDER_QUERY" in
      forward)
        case "$RENDER_SOURCE" in
          "$RENDER_WORK"/src/*) kind=1 ;;
          "$RENDER_WORK"/overlay/*) kind=2 ;;
          *) kind=3 ;;
        esac
        echo "$RENDER_LINE $kind $RENDER_COLUMN 100 10" > "$RENDER_RESULT" ;;
      inverse)
        echo "$RENDER_PAGE 7 $RENDER_WORK/overlay/inc.txt" > "$RENDER_RESULT" ;;
    esac
  |]

uriOf :: FilePath -> Text
uriOf = getUri . filePathToUri
