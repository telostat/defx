module Defx.Programs.ComputeDailyCrosses where

import           Control.Monad.Except   (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson             ((.=))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import           Data.Time              (Day, UTCTime(utctDay))
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
  let currencies = profileCurrencies profile
  filepath <- profileDailyDataFile profile date
  dailyRates <- liftIO (Aeson.eitherDecodeFileStrict' (P.toFilePath filepath)) >>= either throwError pure
  crosses <- computeCrosses (dailyRatesRates dailyRates) (mkPairs (NE.toList currencies))
  liftIO (BL.putStr (Aeson.encode (Aeson.object ["date" .= utctDay (dailyRatesTime dailyRates), "rates" .= crosses])))
