{-# LANGUAGE GADTs #-}

module TestLib.Events (
  untilOutput
  , untilStatus
  , fullBytes
  , patchBytes
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits (symbolVal)
import Language.LSP.Protocol.Message
import Language.LSP.Test
import Render.Protocol
import Test.Sandwich


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

-- | The statuses and log lines up to the next output, and the output.
untilOutput :: Session (ExampleT context IO) ([StatusParams], [LogParams], OutputParams)
untilOutput = go [] []
  where
    go statuses logs = anyMessage >>= \msg -> case classify msg of
      EvOutput o -> return (reverse statuses, reverse logs, o)
      EvStatus s | statusStatus s == StatusErrored -> liftIO $ expectationFailure [i|render failed (exit #{statusExitCode s}); log: #{[logText l | l <- reverse logs]}|]
      EvStatus s -> go (s : statuses) logs
      EvLog l -> go statuses (l : logs)
      EvOther -> go statuses logs

-- | The log lines up to the first status matching the predicate, and that status.
untilStatus :: (StatusParams -> Bool) -> Session (ExampleT context IO) ([LogParams], StatusParams)
untilStatus p = go []
  where
    go logs = anyMessage >>= \msg -> case classify msg of
      EvStatus s | p s -> return (reverse logs, s)
      EvLog l -> go (l : logs)
      _ -> go logs

fullBytes :: OutputParams -> ByteString
fullBytes out@(OutputParams {outputFiles = [OutputFile {outputFileEncoding = EncodingFull}]}) = fileBytes out
fullBytes _ = error "expected a full output"

patchBytes :: OutputParams -> ByteString
patchBytes out@(OutputParams {outputFiles = [OutputFile {outputFileEncoding = EncodingXdelta3}]}) = fileBytes out
patchBytes _ = error "expected an xdelta3 output"

fileBytes :: OutputParams -> ByteString
fileBytes (OutputParams {outputFiles = [f]}) = B64.decodeLenient (T.encodeUtf8 (outputFileBytes f))
fileBytes _ = error "expected exactly one output file"
