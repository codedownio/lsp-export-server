module Main (main) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Options.Applicative
import Render.Command
import Render.Protocol (LogStream(..))
import Render.Server (runRenderServer)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import UnliftIO (try)


data Common = Common {
  commonPrograms :: Programs
  , commonOptions :: [(Text, Text)]
  }

data Invocation =
  Serve Common
  | RenderOnce Common FilePath FilePath

commonParser :: Parser Common
commonParser = Common
  <$> (Programs
    <$> strOption (long "render" <> metavar "EXE" <> help "The program that renders: run in the document's directory with the RENDER_* variables; writes RENDER_OUTPUT")
    <*> optional (strOption (long "settle" <> metavar "EXE" <> help "The program for the settle pass once edits stay quiet; without one there is no settle pass"))
    <*> optional (strOption (long "locate" <> metavar "EXE" <> help "The program that maps source positions to places in the output and back; without one, outputs carry no position")))
  <*> many (option (eitherReader keyValue) (long "option" <> short 'o' <> metavar "KEY=VALUE" <> help "An option passed on as RENDER_OPT_KEY"))
  where
    keyValue s = case break (== '=') s of
      (k, '=' : v) | not (null k) -> Right (T.pack k, T.pack v)
      _ -> Left "expected KEY=VALUE"

invocationParser :: Parser Invocation
invocationParser = hsubparser (
  command "serve" (info (Serve <$> commonParser) (progDesc "Run the language server on stdio"))
  <> command "render" (info (RenderOnce <$> commonParser
                                        <*> strArgument (metavar "INPUT")
                                        <*> strArgument (metavar "OUTPUT"))
                            (progDesc "Render a document from disk once and exit"))
  )

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  execParser (info (invocationParser <**> helper) (fullDesc <> progDesc "Renders documents, live from a language client or once from disk")) >>= \case
    Serve (Common {..}) -> do
      tmp <- getCanonicalTemporaryDirectory
      withTempDirectory tmp "lsp-export-server" $ \workRoot ->
        runRenderServer commonPrograms workRoot (M.fromList commonOptions) >>= \case
          0 -> return ()
          n -> exitWith (ExitFailure n)
    RenderOnce (Common {..}) input output -> do
      inputPath <- makeAbsolute input
      text <- T.readFile inputPath
      tmp <- getCanonicalTemporaryDirectory
      epoch <- round <$> getPOSIXTime
      withTempDirectory tmp "lsp-export-server" $ \workDir -> do
        result <- try $ render commonPrograms RenderRequest {
          requestEntryPath = inputPath
          , requestEntryText = text
          , requestOverlays = []
          , requestWorkDir = workDir
          , requestOptions = M.insert "format" (T.pack (drop 1 (takeExtension output))) (M.fromList commonOptions)
          , requestSettle = True
          , requestEpoch = epoch
          , requestLog = \stream line -> T.hPutStrLn (handleFor stream) line
          }
        case result of
          Left (RenderFailure code msg) -> do
            T.hPutStrLn stderr msg
            exitWith (ExitFailure (case code of { 0 -> 1; n -> n }))
          Right (RenderResult {resultBytes}) -> do
            createDirectoryIfMissing True (takeDirectory output)
            B.writeFile output resultBytes
  where
    handleFor = \case
      LogStdout -> stdout
      LogStderr -> stderr
