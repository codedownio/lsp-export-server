{-# LANGUAGE DeriveGeneric #-}

-- The wire protocol between a render client (the codedown runner) and this server. Commands go
-- client -> server as workspace/executeCommand; results come back as custom notifications.
module Render.Protocol (
  commandStart
  , commandStop
  , commandRebuild
  , commandSetPaused
  , commandResync
  , commandLocate
  , commandSourceAt
  , allCommands

  , methodStatus
  , methodOutput
  , methodLog

  , StartParams(..)
  , StartResult(..)
  , RebuildParams(..)
  , LocateParams(..)
  , SourceAtParams(..)
  , SourceLocation(..)
  , PageBox(..)

  , Status(..)
  , Phase(..)
  , StatusParams(..)
  , OutputEncoding(..)
  , OutputFile(..)
  , OutputParams(..)
  , LogStream(..)
  , LogParams(..)
  , Timing(..)
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)


commandStart, commandStop, commandRebuild, commandSetPaused, commandResync, commandLocate, commandSourceAt :: Text
commandStart = "codedown.render.start"
commandStop = "codedown.render.stop"
commandRebuild = "codedown.render.rebuild"
commandSetPaused = "codedown.render.setPaused"
commandResync = "codedown.render.resync"
commandLocate = "codedown.render.locate"
commandSourceAt = "codedown.render.sourceAt"

allCommands :: [Text]
allCommands = [commandStart, commandStop, commandRebuild, commandSetPaused, commandResync, commandLocate, commandSourceAt]

methodStatus, methodOutput, methodLog :: Text
methodStatus = "codedown/render/status"
methodOutput = "codedown/render/output"
methodLog = "codedown/render/log"

-- Field names are the record field with its prefix dropped, lower-cased: startTaskId -> taskId
fieldOptions :: Int -> Options
fieldOptions n = defaultOptions {
  fieldLabelModifier = lowerFirst . drop n
  , omitNothingFields = True
  }

-- Constructor names with their prefix dropped, lower-cased: StatusReady -> ready
tagOptions :: Int -> Options
tagOptions n = defaultOptions { constructorTagModifier = map toLower . drop n }

lowerFirst :: String -> String
lowerFirst (c:rest) = toLower c : rest
lowerFirst [] = []

data StartParams = StartParams {
  startTaskId :: Text
  , startUri :: Text
  , startFormat :: Text
  , startOptions :: Map Text Text
  , startDebounceMs :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON StartParams where toJSON = genericToJSON (fieldOptions 5)
instance FromJSON StartParams where parseJSON = genericParseJSON (fieldOptions 5)

newtype StartResult = StartResult {
  startResultTaskId :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON StartResult where toJSON = genericToJSON (fieldOptions 11)
instance FromJSON StartResult where parseJSON = genericParseJSON (fieldOptions 11)

newtype RebuildParams = RebuildParams {
  rebuildFiles :: [FilePath]
  } deriving (Show, Eq, Generic)
instance ToJSON RebuildParams where toJSON = genericToJSON (fieldOptions 7)
instance FromJSON RebuildParams where parseJSON = genericParseJSON (fieldOptions 7)

-- | A source position, zero-based like LSP positions
data LocateParams = LocateParams {
  locateTaskId :: Text
  , locateUri :: Text
  , locateLine :: Int
  , locateCharacter :: Int
  } deriving (Show, Eq, Generic)
instance ToJSON LocateParams where toJSON = genericToJSON (fieldOptions 6)
instance FromJSON LocateParams where parseJSON = genericParseJSON (fieldOptions 6)

-- | A point on a page of the last output: a one-based page and PDF points from its top-left corner
data SourceAtParams = SourceAtParams {
  sourceAtTaskId :: Text
  , sourceAtPage :: Int
  , sourceAtX :: Double
  , sourceAtY :: Double
  } deriving (Show, Eq, Generic)
instance ToJSON SourceAtParams where toJSON = genericToJSON (fieldOptions 8)
instance FromJSON SourceAtParams where parseJSON = genericParseJSON (fieldOptions 8)

-- | Zero-based like LSP positions
data SourceLocation = SourceLocation {
  sourceUri :: Text
  , sourceLine :: Int
  , sourceCharacter :: Int
  } deriving (Show, Eq, Generic)
instance ToJSON SourceLocation where toJSON = genericToJSON (fieldOptions 6)
instance FromJSON SourceLocation where parseJSON = genericParseJSON (fieldOptions 6)

-- | A box on a page of the output: a one-based page, and PDF points from the page's top-left corner
data PageBox = PageBox {
  boxPage :: Int
  , boxLeft :: Double
  , boxTop :: Double
  , boxWidth :: Double
  , boxHeight :: Double
  } deriving (Show, Eq, Generic)
instance ToJSON PageBox where toJSON = genericToJSON (fieldOptions 3)
instance FromJSON PageBox where parseJSON = genericParseJSON (fieldOptions 3)

data Timing = Timing {
  timingLabel :: Text
  , timingMs :: Double
  } deriving (Show, Eq, Generic)
instance ToJSON Timing where toJSON = genericToJSON (fieldOptions 6)
instance FromJSON Timing where parseJSON = genericParseJSON (fieldOptions 6)

data Status = StatusRendering | StatusReady | StatusErrored
  deriving (Show, Eq, Generic)
instance ToJSON Status where toJSON = genericToJSON (tagOptions 6)
instance FromJSON Status where parseJSON = genericParseJSON (tagOptions 6)

-- | The quick pass runs right after the debounce; the settle pass follows once edits stay quiet
data Phase = PhaseQuick | PhaseSettle
  deriving (Show, Eq, Generic)
instance ToJSON Phase where toJSON = genericToJSON (tagOptions 5)
instance FromJSON Phase where parseJSON = genericParseJSON (tagOptions 5)

data StatusParams = StatusParams {
  statusTaskId :: Text
  , statusStatus :: Status
  , statusPhase :: Phase
  -- | What made the render run, e.g. the edit it answers
  , statusReason :: Maybe Text
  , statusExitCode :: Maybe Int
  , statusRenderMs :: Maybe Double
  , statusTimings :: [Timing]
  } deriving (Show, Eq, Generic)
instance ToJSON StatusParams where toJSON = genericToJSON (fieldOptions 6)
instance FromJSON StatusParams where parseJSON = genericParseJSON (fieldOptions 6)

data OutputEncoding = EncodingFull | EncodingXdelta3
  deriving (Show, Eq, Generic)
instance ToJSON OutputEncoding where toJSON = genericToJSON (tagOptions 8)
instance FromJSON OutputEncoding where parseJSON = genericParseJSON (tagOptions 8)

data OutputFile = OutputFile {
  outputFileName :: Text
  , outputFileEncoding :: OutputEncoding
  -- | base64
  , outputFileBytes :: Text
  , outputFileDecodedSize :: Int
  } deriving (Show, Eq, Generic)
instance ToJSON OutputFile where toJSON = genericToJSON (fieldOptions 10)
instance FromJSON OutputFile where parseJSON = genericParseJSON (fieldOptions 10)

data OutputParams = OutputParams {
  outputTaskId :: Text
  , outputRenderId :: Int
  , outputBaseRenderId :: Maybe Int
  , outputDocumentVersion :: Maybe Int32
  , outputFiles :: [OutputFile]
  , outputDeps :: [FilePath]
  , outputRenderMs :: Double
  , outputTimings :: [Timing]
  -- | Where the edit this render answers landed in the output
  , outputPosition :: Maybe PageBox
  } deriving (Show, Eq, Generic)
instance ToJSON OutputParams where toJSON = genericToJSON (fieldOptions 6)
instance FromJSON OutputParams where parseJSON = genericParseJSON (fieldOptions 6)

data LogStream = LogStdout | LogStderr
  deriving (Show, Eq, Generic)
instance ToJSON LogStream where toJSON = genericToJSON (tagOptions 3)
instance FromJSON LogStream where parseJSON = genericParseJSON (tagOptions 3)

data LogParams = LogParams {
  logTaskId :: Text
  , logStream :: LogStream
  , logText :: Text
  , logClear :: Bool
  } deriving (Show, Eq, Generic)
instance ToJSON LogParams where toJSON = genericToJSON (fieldOptions 3)
instance FromJSON LogParams where parseJSON = genericParseJSON (fieldOptions 3)

