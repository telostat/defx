module Defx.Providers.Oxr where

import           Control.Monad.Except       (MonadError(throwError))
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 ((.:))
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           ((.:?))
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (Day)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Defx.Types                 (Currency, DailyRates(DailyRates), Rates)
import qualified Network.HTTP.Simple        as NHS
import           Text.Printf                (printf)


-- | Type encoding for latest/historical OXR endpoint response value.
--
-- >>> Aeson.decode "{\"disclaimer\":\"A\",\"license\":\"B\",\"timestamp\":1,\"base\":\"USD\",\"rates\":{\"AED\":3.672538,\"AFN\":66.809999}}" :: Maybe OxrResponseValue
-- Just (OxrResponseValue {oxrResponseValueDisclaimer = "A", oxrResponseValueLicense = "B", oxrResponseValueTimestamp = 1970-01-01 00:00:01 UTC, oxrResponseValueBase = "USD", oxrResponseValueRates = fromList [("AED",MkValue {unValue = 3.67253800}),("AFN",MkValue {unValue = 66.80999900})]})
data OxrResponseValue = OxrResponseValue
  { oxrResponseValueDisclaimer :: !T.Text   -- ^ Disclaimer text
  , oxrResponseValueLicense    :: !T.Text   -- ^ License text
  , oxrResponseValueTimestamp  :: !UTCTime  -- ^ Timestamp
  , oxrResponseValueBase       :: !Currency -- ^ Base currency
  , oxrResponseValueRates      :: !Rates    -- ^ Rates table
  } deriving (Show)


instance Aeson.FromJSON OxrResponseValue where
  parseJSON = Aeson.withObject "DailyRates" $ \o -> OxrResponseValue
    <$> o .: "disclaimer"
    <*> o .: "license"
    <*> (posixSecondsToUTCTime . fromInteger <$> o .: "timestamp")
    <*> o .: "base"
    <*> o .: "rates"


-- | Attempts to retrieve daily rates from remote OXR API endpoint.
retrieveOxrDailyRates
  :: (MonadError String m, MonadIO m)
  => String   -- ^ OXR API key
  -> Day      -- ^ Date of historical rates
  -> Currency -- ^ Base currency
  -> m OxrResponseValue
retrieveOxrDailyRates apikey date base = do
  response <- NHS.httpLBS (addParams request)
  case NHS.getResponseStatusCode response of
    200 -> case Aeson.eitherDecode (NHS.getResponseBody response) of
      Left err -> throwError ("Cannot convert retrieved data. Error was: " <> show err)
      Right rd -> pure rd
    err -> throwError ("Cannot retrieve remote data. Error code was: " <> show err <> ". Error was: " <> BLC.unpack (NHS.getResponseBody response))
  where
    request = NHS.parseRequest_ (printf "https://openexchangerates.org/api/historical/%s.json" (show date))
    addParams = NHS.setRequestQueryString [("base", Just $ TE.encodeUtf8 base), ("app_id", Just $ BC.pack apikey)]


-- | Attempts to retrieve daily rates from remote OXR API endpoint and return as
-- standard 'DailyRates' value.
retrieveDailyRates
  :: (MonadError String m, MonadIO m)
  => String   -- ^ OXR API key
  -> Day      -- ^ Date of historical rates
  -> Currency -- ^ Base currency
  -> m DailyRates
retrieveDailyRates apikey date base = do
  oxrvalue <- retrieveOxrDailyRates apikey date base
  pure $ DailyRates date base (oxrResponseValueRates oxrvalue) (Just "OXR") (Just (oxrResponseValueTimestamp oxrvalue))
