module Defx.Programs.DownloadDailyRates where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text              as T
import           Data.Time              (Day)
import           Defx.Config
                 ( Profile(..)
                 , ProviderConfig(..)
                 , ensureProfileDailyDataDirectory
                 , profileDailyDataFile
                 , readConfigFileWithProfile
                 )
import           Defx.IO                (toAbsFilePath, writeDailyRates)
import           Defx.Providers.Oxr     (retrieveDailyRates)
import           Defx.Types             (DailyRates)
import qualified Path                   as P


-- | Attempts to download remote daily rates for a given date and write it into a file.
doDownloadDailyRates
  :: (MonadError String m, MonadIO m)
  => FilePath  -- ^ Path to the configuration file
  -> T.Text    -- ^ Profile name
  -> Day       -- ^ Date of historical rates
  -> m ()
doDownloadDailyRates config pname date = do
  configPath <- toAbsFilePath True config
  (_config, profile) <- readConfigFileWithProfile configPath pname
  downloadDailyRates profile date


-- | Attempts to download remote daily rates for a given date range and write it into a file.
doDownloadDailyRatesRange
  :: (MonadError String m, MonadIO m)
  => FilePath  -- ^ Path to the configuration file
  -> T.Text    -- ^ Profile name
  -> Day       -- ^ Date of historical rates to download since (inclusive)
  -> Day       -- ^ Date of historical rates to download until (inclusive)
  -> m ()
doDownloadDailyRatesRange config pname dateSince dateUntil = do
  configPath <- toAbsFilePath True config
  (_config, profile) <- readConfigFileWithProfile configPath pname
  mapM_ (downloadDailyRates profile) [dateSince..dateUntil]


-- | Attempts to download daily rates for the given date for a given profile.
downloadDailyRates
  :: (MonadError String m, MonadIO m)
  => Profile
  -> Day
  -> m ()
downloadDailyRates profile date = do
  let retrieve = getRetriever (profileProvider profile)
  _ <- ensureProfileDailyDataDirectory profile
  outpath <- profileDailyDataFile profile date
  retrieve date >>= (`writeDailyRates` P.toFilePath outpath)


-- | Builds a historical rates retriever for the given provider and returns it.
getRetriever :: (MonadError String m, MonadIO m) => ProviderConfig -> (Day -> m DailyRates)
getRetriever (ProviderConfigOxr c) = retrieveDailyRates c
