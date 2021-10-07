module Main where

import           Control.Monad.Except              (ExceptT, runExceptT)
import qualified Data.Text                         as T
import           Data.Time                         (Day)
import           Data.Version                      (showVersion)
import           Defx.Programs.ComputeDailyCrosses (doComputeDailyCrosses)
import           Defx.Programs.DownloadDailyRates  (doDownloadDailyRates, doDownloadDailyRatesRange)
import           Defx.Types                        (Currency)
import qualified Options.Applicative               as OA
import qualified Path                              as P
import           Paths_defx                        (version)
import           System.Exit                       (die, exitSuccess)
import           System.IO                         (hPutStrLn, stderr)


-- | Main entry-point of the application.
main :: IO ()
main = cliProgram =<< OA.execParser cliProgramParserInfo


-- | Runs the CLI program.
cliProgram :: CliArguments -> IO ()
cliProgram (CliArguments (DownloadDailyRates config profile date)) =
  doRun (doDownloadDailyRates config profile date)
cliProgram (CliArguments (DownloadDailyRatesRange config profile since until)) =
  doRun (doDownloadDailyRatesRange config profile since until)
cliProgram (CliArguments (ComputeDailyCrosses config profile date)) =
  doRun (doComputeDailyCrosses config profile date)


-- | Registry of commands.
data Command =
    DownloadDailyRates !FilePath !T.Text !Day
  | DownloadDailyRatesRange !FilePath !T.Text !Day !Day
  | ComputeDailyCrosses  !FilePath !T.Text !Day
  deriving Show


-- | Parses program arguments.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  ( OA.command "get-daily" (OA.info
      (DownloadDailyRates
        <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")
        <*> OA.strOption (OA.long "profile" <> OA.metavar "PROFILE-NAME" <> OA.help "Name of the configuration profile")
        <*> (read <$> OA.strOption (OA.long "date" <> OA.metavar "DATE" <> OA.help "Date to download rates for"))
      )
      (OA.progDesc "Download daily FX rates for a given date")
    )
  <> OA.command "get-daily-range" (OA.info
      (DownloadDailyRatesRange
        <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")
        <*> OA.strOption (OA.long "profile" <> OA.metavar "PROFILE-NAME" <> OA.help "Name of the configuration profile")
        <*> (read <$> OA.strOption (OA.long "since" <> OA.metavar "SINCE-DATE" <> OA.help "Date to download rates since (inclusive)"))
        <*> (read <$> OA.strOption (OA.long "until" <> OA.metavar "UNTIL-DATE" <> OA.help "Date to download rates until (inclusive)"))
      )
      (OA.progDesc "Download daily FX rates for a given date range")
    )
  <> OA.command "compute-daily-crosses" (OA.info
      (ComputeDailyCrosses
        <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")
        <*> OA.strOption (OA.long "profile" <> OA.metavar "PROFILE-NAME" <> OA.help "Name of the configuration profile")
        <*> (read <$> OA.strOption (OA.long "date" <> OA.metavar "DATE" <> OA.help "Date to compute crosses for"))
      )
      (OA.progDesc "Computes daily FX rates for a given date")
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


-- * Helpers
-- &helpers

-- | Helper to run DEFX command line programs.
doRun :: ExceptT String IO () -> IO ()
doRun action = either onError (const exitSuccess) =<< runExceptT action
  where
    onError err = hPutStrLn stderr err >> die "Exiting..."
