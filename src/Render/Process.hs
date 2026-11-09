module Render.Process (
  runCapturing
  ) where

import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (ExitCode)
import System.IO
import System.Posix.Signals (sigTERM, signalProcessGroup)
import System.Process
import UnliftIO (IOException, SomeException, bracket, handle, race_, throwIO, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)


-- | Run a command in @cwd@ with the given environment, feeding each output line to the callback,
-- and return its exit code. The command runs in its own process group so that a cancellation
-- (an async exception here) ends the whole tree, latexmk's children included.
runCapturing :: FilePath -> [(String, String)] -> FilePath -> [String] -> (Bool -> T.Text -> IO ()) -> IO ExitCode
runCapturing cwd env exe args onLine = do
  let cp = (proc exe args) {
        cwd = Just cwd
        , env = Just env
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }
  bracket (createProcess cp) cleanup $ \case
    (_, Just hOut, Just hErr, ph) -> do
      hSetBinaryMode hOut False
      hSetBinaryMode hErr False
      hSetEncoding hOut utf8
      hSetEncoding hErr utf8
      withAsync (pump hOut (onLine True)) $ \outA ->
        withAsync (pump hErr (onLine False)) $ \errA -> do
          code <- waitForProcess ph
          wait outA
          wait errA
          return code
    _ -> throwIO (userError "createProcess gave no pipes")
  where
    pump h f = handle (\(_ :: IOException) -> return ()) $ forever $ T.hGetLine h >>= f

    -- Only a process that is still running (we were cancelled) needs its group ended
    cleanup (_, mOut, mErr, ph) = do
      getProcessExitCode ph >>= \case
        Just _ -> return ()
        Nothing -> do
          getPid ph >>= \case
            Just pid -> handle (\(_ :: SomeException) -> return ()) $ signalProcessGroup sigTERM pid
            Nothing -> return ()
          race_ (waitForProcess ph) (threadDelay 2_000_000)
          terminateProcess ph
      mapM_ hClose mOut
      mapM_ hClose mErr
