module Main where

import           Control.Monad.Except          (runExceptT)
import qualified Data.Text                     as T
import           Data.Time                     (Day)
import           Data.Version                  (showVersion)
import           Defx.Programs.HistoricalRates (downloadDailyRates)
import           Defx.Types                    (Currency)
import qualified Options.Applicative           as OA
import           Paths_defx                    (version)
import           System.Exit                   (die, exitSuccess)
import           System.IO                     (hPutStrLn, stderr)


-- | Main entry-point of the application.
main :: IO ()
main = cliProgram =<< OA.execParser cliProgramParserInfo


-- | Runs the CLI program.
cliProgram :: CliArguments -> IO ()
cliProgram (CliArguments (OxrDownloadHistorical apikey date base path)) = do
  result <- runExceptT (downloadDailyRates apikey date base path)
  case result of
    Left err -> hPutStrLn stderr err >> die "Exiting..."
    Right () -> exitSuccess


-- | Registry of commands.
data Command =
    OxrDownloadHistorical !String !Day !Currency !FilePath
  deriving Show


-- | Parses program arguments.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  ( OA.command "oxr-historical" (OA.info
      (OxrDownloadHistorical
        <$> OA.strOption (OA.long "api-key" <> OA.metavar "API-KEY" <> OA.help "OXR API key")
        <*> (read <$> OA.strOption (OA.long "date" <> OA.metavar "DATE" <> OA.help "Date to download rates for"))
        <*> (T.pack <$> OA.strOption (OA.long "base" <> OA.metavar "BASE-CCY" <> OA.value "USD" <> OA.showDefault <> OA.help "Base currency"))
        <*> OA.strOption (OA.long "output" <> OA.metavar "OUTPUT" <> OA.help "Output file path")
      )
      (OA.progDesc "Download historical FX rates from OXR for a given date")
    )
  )


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command } deriving Show


-- | Version option parser.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")


-- | Program parser information.
cliProgramParserInfo :: OA.ParserInfo CliArguments
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "DEFX Program" <> OA.header "defx")
