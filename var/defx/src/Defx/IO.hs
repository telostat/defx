module Defx.IO where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import           Defx.Types             (DailyRates)
import qualified System.IO              as IO


-- | Writes given daily rates to the given IO handle.
hPutDailyRates :: MonadIO m => DailyRates -> IO.Handle -> m ()
hPutDailyRates v h = liftIO (BL.hPut h (Aeson.encode v))


-- | Writes given daily rates to the file at the given path.
writeDailyRates :: MonadIO m => DailyRates -> FilePath -> m ()
writeDailyRates v p = liftIO (IO.withFile p IO.WriteMode (hPutDailyRates v))
