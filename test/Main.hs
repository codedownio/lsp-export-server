module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, decode, encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.String.Interpolate
import Render.Backend.Latex (depsFromFls)
import Render.Protocol
import qualified Render.Xdelta as Xdelta
import Test.Sandwich


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

  describe "latex deps" $ do
    it "maps the recorder's inputs back to the documents they stand for" $ do
      let fls = [__i|PWD /home/u/docs
                     INPUT /nix/store/abc-texlive/article.cls
                     INPUT /work/src/main.tex
                     INPUT ./sub/inc.tex
                     INPUT /work/overlay/sub/other.tex
                     INPUT /work/out/main.aux
                     INPUT /home/u/other/img.png|]
      depsFromFls "/home/u/docs" "/home/u/docs/main.tex" "/work/src" "/work/overlay" "/work" fls
        `shouldBe` ["/home/u/docs/main.tex", "/home/u/docs/sub/inc.tex", "/home/u/docs/sub/other.tex", "/home/u/other/img.png"]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
