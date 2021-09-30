{-# LANGUAGE DataKinds #-}

module Defx.Types where

import           Data.Aeson            ((.:), (.:?), (.=))
import qualified Data.Aeson            as Aeson
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (fromJust)
import           Data.Scientific       (FPFormat(Fixed), Scientific, formatScientific)
import qualified Data.Text             as T
import           Data.Time             (Day, UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Numeric.Decimal       (Decimal, RoundHalfUp, fromScientificDecimal)


-- | Type encoding for currency codes.
type Currency = T.Text


-- | Type encoding for currency pairs.
type Pair = (Currency, Currency)


-- | Type encoding of a foreign exchange rate observation.
data FXRate = FXRate
  { fxRateDate :: !Day
  , fxRatePair :: !Pair
  , fxRateRate :: !Value
  } deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'FXRate'.
--
-- >>> Aeson.decode "{\"date\": \"2021-01-01\", \"ccy1\": \"EUR\", \"ccy2\": \"USD\", \"rate\": 1.123456789}" :: Maybe FXRate
-- Just (FXRate {fxRateDate = 2021-01-01, fxRatePair = ("EUR","USD"), fxRateRate = MkValue {unValue = 1.12345679}})
instance Aeson.FromJSON FXRate where
  parseJSON = Aeson.withObject "FXRate" $ \o -> FXRate
    <$> o .: "date"
    <*> ((,) <$> o .: "ccy1" <*> o .: "ccy2")
    <*> o .: "rate"


-- | 'Aeson.ToJSON' instance for 'FXRate'.
--
-- >>> Aeson.encode (FXRate {fxRateDate = read "2021-01-01", fxRatePair = ("EUR", "USD"), fxRateRate = mkValueLossy 1.123456789})
-- "{\"ccy2\":\"USD\",\"date\":\"2021-01-01\",\"rate\":\"1.12345679\",\"ccy1\":\"EUR\"}"
instance Aeson.ToJSON FXRate where
  toJSON x = Aeson.object
    [ "date" .= fxRateDate x
    , "ccy1" .= fst (fxRatePair x)
    , "ccy2" .= snd (fxRatePair x)
    , "rate" .= fxRateRate x
    ]


-- | Type encoding for rate values.
newtype Value = MkValue { unValue :: Decimal RoundHalfUp 8 Integer }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Value'.
--
-- >>> Aeson.decode "1.12345678" :: Maybe Value
-- Just (MkValue {unValue = 1.12345678})
--
-- >>> Aeson.decode "1.123456789" :: Maybe Value
-- Just (MkValue {unValue = 1.12345679})
instance Aeson.FromJSON Value where
  parseJSON = Aeson.withScientific "RateValue" (pure . mkValueLossy)


-- | 'Aeson.ToJSON' instance for 'Value'.
--
-- >>> Aeson.encode (mkValueLossy 1.12345678)
-- "\"1.12345678\""
instance Aeson.ToJSON Value where
  toJSON = Aeson.String . T.pack . show . unValue


-- | Smart constructor for 'Value' type from 'Scientic' values.
--
-- >>> mkValue 1.12345678 :: Maybe Value
-- Just (MkValue {unValue = 1.12345678})
--
-- >>> mkValue 1.123456784 :: Maybe Value
-- Nothing
--
-- >>> mkValue 1.123456785 :: Maybe Value
-- Nothing
--
-- >>> mkValue 1.123456786 :: Maybe Value
-- Nothing
mkValue :: MonadFail m => Scientific -> m Value
mkValue x = maybe err (pure . MkValue) (fromScientificDecimal x)
  where
    err = fail ("Can not convert scientific to rate value: " <> show x)


-- | Lossy constructor for 'Value' type from 'Scientic' values.
--
-- >>> mkValueLossy 1.12345678 :: Value
-- MkValue {unValue = 1.12345678}
--
-- >>> mkValueLossy 1.123456784 :: Value
-- MkValue {unValue = 1.12345678}
--
-- >>> mkValueLossy 1.123456785 :: Value
-- MkValue {unValue = 1.12345678}
--
-- >>> mkValueLossy 1.123456786 :: Value
-- MkValue {unValue = 1.12345679}
mkValueLossy :: Scientific -> Value
mkValueLossy = MkValue . fromJust . fromScientificDecimal . read . formatScientific Fixed (Just 8)


-- | Type encoding for a collection of rates.
type Rates = HM.HashMap Currency Value


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
