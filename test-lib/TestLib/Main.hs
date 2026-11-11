module TestLib.Main (
  specMain
  ) where

import Data.Time
import Test.Sandwich


specMain :: TopSpec -> IO ()
specMain = runSandwichWithCommandLineArgs options
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (formatTime defaultTimeLocale "%Y-%m-%dT%H.%M.%S" <$> getCurrentTime)
      }
