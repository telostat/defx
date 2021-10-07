module Defx.Programs.ComputeDailyCrosses where

import           Control.Monad.Except   (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson             ((.=))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import           Data.Time              (Day, UTCTime(utctDay))
import qualified Data.Vector            as V
import           Defx.Config            (Profile(..), profileDailyDataFile, readConfigFileWithProfile)
import           Defx.IO                (toAbsFilePath)
import           Defx.Machinery         (computeCrosses, mkPairs)
import           Defx.Types             (DailyRates(..))
import qualified Path                   as P


doComputeDailyCrosses
  :: (MonadError String m, MonadIO m)
  => FilePath  -- ^ Path to the configuration file
  -> T.Text    -- ^ Profile name
  -> Day       -- ^ Date to compute crosses for
  -> m ()
doComputeDailyCrosses config pname date = do
  configPath <- toAbsFilePath True config
  (_config, profile) <- readConfigFileWithProfile configPath pname
  value <- computeDailyCrosses profile date
  liftIO (BL.putStr (Aeson.encode value))


doComputeDailyCrossesRange
  :: (MonadError String m, MonadIO m)
  => FilePath  -- ^ Path to the configuration file
  -> T.Text    -- ^ Profile name
  -> Day       -- ^ Date to compute crosses since (inclusive)
  -> Day       -- ^ Date to compute crosses until (inclusive)
  -> m ()
doComputeDailyCrossesRange config pname dateSince dateUntil = do
  configPath <- toAbsFilePath True config
  (_config, profile) <- readConfigFileWithProfile configPath pname
  values <- Aeson.Array . V.fromList <$> mapM (computeDailyCrosses profile) [dateSince..dateUntil]
  liftIO (BL.putStr (Aeson.encode values))


computeDailyCrosses
  :: (MonadError String m, MonadIO m)
  => Profile  -- ^ Profile to compute daily crosses for.
  -> Day      -- ^ Date to compute crosses for
  -> m Aeson.Value
computeDailyCrosses profile date = do
  let currencies = profileCurrencies profile
  filepath <- profileDailyDataFile profile date
  dailyRates <- liftIO (Aeson.eitherDecodeFileStrict' (P.toFilePath filepath)) >>= either throwError pure
  crosses <- computeCrosses (dailyRatesRates dailyRates) (mkPairs (NE.toList currencies))
  pure (Aeson.object ["date" .= utctDay (dailyRatesTime dailyRates), "rates" .= crosses])
