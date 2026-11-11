module Spec.Protocol (tests, main) where

import Data.Aeson (Value, decode, encode)
import qualified Data.Map as M
import Data.String.Interpolate
import Render.Protocol
import Test.Sandwich
import TestLib.Main


tests :: SpecFree context IO ()
tests = describe "protocol" $ do
  it "writes camelCase fields and lower-case tags" $ do
    let params = StatusParams "t1" StatusErrored PhaseQuick (Just "edit to x.tex (v3)") (Just 12) Nothing []
    decode (encode params) `shouldBe` (decode [i|
      {"taskId":"t1","status":"errored","phase":"quick","reason":"edit to x.tex (v3)","exitCode":12,"timings":[]}
    |] :: Maybe Value)

  it "reads start params" $ do
    decode [i|
      {"taskId":"a","uri":"file:///home/x.tex","format":"pdf","options":{"engine":"xelatex"},"debounceMs":50}
    |] `shouldBe` Just (StartParams "a" "file:///home/x.tex" "pdf" (M.fromList [("engine", "xelatex")]) (Just 50))

main :: IO ()
main = specMain tests
