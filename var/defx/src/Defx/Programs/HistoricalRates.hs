module Defx.Programs.HistoricalRates where
import Control.Monad.Except   (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Time              (Day)
import Defx.IO                (writeDailyRates)
import Defx.Providers.Oxr     (retrieveDailyRates)
import Defx.Types             (Currency)


-- | Attempts to download remote OXR daily rates and write it into a file.
doDownloadDailyRates
  :: (MonadError String m, MonadIO m)
  => String   -- ^ OXR API key
  -> Day      -- ^ Date of historical rates
  -> Currency -- ^ Base currency
  -> FilePath -- ^ Output file path
  -> m ()
doDownloadDailyRates apikey date base path = do
  dailyrates <- retrieveDailyRates apikey date base
  writeDailyRates dailyrates path
