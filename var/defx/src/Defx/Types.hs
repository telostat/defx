module Defx.Types where

import           Data.Aeson            ((.:), (.:?), (.=))
import qualified Data.Aeson            as Aeson
import qualified Data.HashMap.Strict   as HM
import           Data.Scientific       (Scientific)
import qualified Data.Text             as T
import           Data.Time             (Day, UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)


-- | Type encoding for currency codes.
type Currency = T.Text


-- | Type encoding for a collection of rates.
type Rates = HM.HashMap Currency Scientific


-- | Type encoding for daily rates for a given base
data DailyRates = DailyRates
  { dailyRatesDate      :: !Day              -- ^ Date of the rates.
  , dailyRatesBase      :: !Currency         -- ^ Base currency.
  , dailyRatesRates     :: !Rates            -- ^ A collection of rates.
  , dailyRatesProvider  :: !(Maybe T.Text)     -- ^ Provider identifier, if any.
  , dailyRatesTimestamp :: !(Maybe UTCTime)  -- ^ Time the rates have been published, if any.
  } deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'DailyRates'.
--
-- >>> Aeson.eitherDecode "{\"date\": \"2001-12-31\", \"time\": 1009843199, \"base\": \"USD\", \"rates\": {}}" :: Either String DailyRates
-- Right (DailyRates {dailyRatesDate = 2001-12-31, dailyRatesBase = "USD", dailyRatesRates = fromList [], dailyRatesProvider = Nothing, dailyRatesTimestamp = Just 2001-12-31 23:59:59 UTC})
instance Aeson.FromJSON DailyRates where
  parseJSON = Aeson.withObject "DailyRates" $ \o -> DailyRates
    <$> o .: "date"
    <*> o .: "base"
    <*> o .: "rates"
    <*> o .:? "provider"
    <*> (fmap (posixSecondsToUTCTime . fromInteger) <$> o .: "timestamp")


-- | 'Aeson.ToJSON' instance for 'DailyRates'
--
-- >>> Aeson.encode $ DailyRates {dailyRatesDate = read "2001-12-31", dailyRatesTimestamp = Just (read "2001-12-31 23:59:59 UTC"), dailyRatesBase = "USD", dailyRatesRates = HM.fromList [], dailyRatesProvider = Nothing}
-- "{\"base\":\"USD\",\"time\":1009843199,\"rates\":{},\"date\":\"2001-12-31\",\"provider\":null}"
instance Aeson.ToJSON DailyRates where
  toJSON x = Aeson.object
    [ "date" .= dailyRatesDate x
    , "base" .= dailyRatesBase x
    , "rates" .= dailyRatesRates x
    , "provider" .= dailyRatesProvider x
    , "timestamp" .= (utcTimeToPOSIXSeconds <$> dailyRatesTimestamp x)
    ]
