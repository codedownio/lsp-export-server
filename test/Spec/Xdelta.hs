module Spec.Xdelta (tests, main) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import qualified Render.Xdelta as Xdelta
import Test.Sandwich
import TestLib.Main


tests :: SpecFree context IO ()
tests = describe "xdelta3" $ do
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

main :: IO ()
main = specMain tests
