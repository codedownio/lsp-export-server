module Spec.Deps (tests, main) where

import Render.Command (mapDeps)
import Test.Sandwich
import TestLib.Main


tests :: SpecFree context IO ()
tests = describe "deps" $ do
  it "maps the work directory's copies back to the documents they stand for" $ do
    let reported = ["/nix/store/abc-texlive/article.cls", "/work/src/main.tex", "./sub/inc.tex", "/work/overlay/sub/other.tex", "/work/out/main.aux", "/home/u/other/img.png"]
    mapDeps "/home/u/docs" "/home/u/docs/main.tex" "/work" reported
      `shouldBe` ["/home/u/docs/main.tex", "/home/u/docs/sub/inc.tex", "/home/u/docs/sub/other.tex", "/home/u/other/img.png"]

main :: IO ()
main = specMain tests
