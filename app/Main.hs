module Main (main) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Render.Backend
import Render.Backend.Latex (latexBackend)
import Render.Backend.Pandoc (pandocBackend)
import Render.Protocol (LogStream(..))
import Render.Server (runRenderServer)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import UnliftIO (try)


backends :: [Backend]
backends = [latexBackend, pandocBackend]

data Common = Common {
  commonBackend :: String
  , commonOptions :: [(Text, Text)]
  }

data Command =
  Serve Common
  | RenderOnce Common FilePath FilePath

commonParser :: Parser Common
commonParser = Common
  <$> strOption (long "backend" <> metavar "NAME" <> help "Which backend renders: latex, pandoc")
  <*> many (option (eitherReader keyValue) (long "option" <> short 'o' <> metavar "KEY=VALUE" <> help "A backend option, e.g. engine=xelatex"))
  where
    keyValue s = case break (== '=') s of
      (k, '=' : v) | not (null k) -> Right (T.pack k, T.pack v)
      _ -> Left "expected KEY=VALUE"

commandParser :: Parser Command
commandParser = hsubparser (
  command "serve" (info (Serve <$> commonParser) (progDesc "Run the language server on stdio"))
  <> command "render" (info (RenderOnce <$> commonParser
                                        <*> strArgument (metavar "INPUT")
                                        <*> strArgument (metavar "OUTPUT"))
                            (progDesc "Render a document from disk once and exit"))
  )

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  cmd <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Renders documents, live from a language client or once from disk"))
  case cmd of
    Serve common -> do
      backend <- pickBackend common
      tmp <- getCanonicalTemporaryDirectory
      withTempDirectory tmp "lsp-export-server" $ \workRoot ->
        runRenderServer backend workRoot (M.fromList (commonOptions common)) >>= \case
          0 -> return ()
          n -> exitWith (ExitFailure n)
    RenderOnce common input output -> do
      backend <- pickBackend common
      inputPath <- makeAbsolute input
      text <- T.readFile inputPath
      tmp <- getCanonicalTemporaryDirectory
      epoch <- round <$> getPOSIXTime
      withTempDirectory tmp "lsp-export-server" $ \workDir -> do
        result <- try $ backendRender backend RenderRequest {
          requestEntryPath = inputPath
          , requestEntryText = text
          , requestOverlays = []
          , requestWorkDir = workDir
          , requestOptions = M.insert "format" (T.pack (drop 1 (takeExtension output))) (M.fromList (commonOptions common))
          , requestSettle = True
          , requestEpoch = epoch
          , requestLog = \stream line -> T.hPutStrLn (if stream == LogStdout then stdout else stderr) line
          }
        case result of
          Left (RenderFailure code msg) -> do
            T.hPutStrLn stderr msg
            exitWith (ExitFailure (if code == 0 then 1 else code))
          Right (RenderResult {resultBytes}) -> do
            createDirectoryIfMissing True (takeDirectory output)
            B.writeFile output resultBytes

pickBackend :: Common -> IO Backend
pickBackend (Common {commonBackend}) = case [b | b <- backends, backendName b == T.pack commonBackend] of
  (b : _) -> return b
  [] -> do
    hPutStrLn stderr ("unknown backend: " <> commonBackend <> "; known: " <> unwords (map (T.unpack . backendName) backends))
    exitWith (ExitFailure 2)
