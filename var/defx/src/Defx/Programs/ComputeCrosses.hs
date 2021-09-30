module Defx.Programs.ComputeCrosses where

import           Control.Exception      (try)
import           Control.Monad          (when, (>=>))
import           Control.Monad.Except   (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Aeson             as Aeson
import qualified Data.HashMap.Strict    as HM
import           Data.List              (nub)
import           Data.Maybe             (fromMaybe)
import           Data.Scientific        (Scientific)
import           Data.Time              (Day)
import           Defx.Types
                 ( Currency
                 , DailyRates(DailyRates, dailyRatesDate, dailyRatesRates)
                 , Pair
                 , Value(MkValue, unValue), FXRate (FXRate)
                 )
import           Numeric.Decimal        (divideDecimalBoundedWithRounding, divideDecimalWithRounding)


doComputeCrosses
  :: (MonadError String m, MonadIO m)
  => [Currency]
  -> FilePath
  -> FilePath
  -> m ()
doComputeCrosses currencies ipath opath = do
  let pairs = mkPairs currencies
  eDailyRates <- liftIO (Aeson.eitherDecodeFileStrict' ipath)
  crosses <- either throwError (computeCrosses pairs) eDailyRates
  liftIO (Aeson.encodeFile opath crosses)


computeCrosses
  :: MonadError String m
  => [Pair]
  -> DailyRates
  -> m [FXRate]
computeCrosses pairs dailyrates = mapM (`computeCross` dailyrates) pairs


computeCross
  :: MonadError String m
  => Pair
  -> DailyRates
  -> m FXRate
computeCross pair@(ccy1, ccy2) dailyrates = do
  let date = dailyRatesDate dailyrates
  rate1 <- findRate ccy1
  rate2 <- findRate ccy2
  pure (FXRate date pair (divideValueWeird rate2 rate1))
  where
    findRate ccy = maybe (throwError ("No rate found for currency: " <> show ccy)) pure (HM.lookup ccy (dailyRatesRates dailyrates))
    divideValueWeird (MkValue a) (MkValue b) = MkValue (fromMaybe 0 (divideDecimalWithRounding a b))


-- | Creates a list of pairs from a given list of currencies.
--
-- >>> mkPairs []
-- []
-- >>> mkPairs ["USD"]
-- []
-- >>> mkPairs ["USD", "EUR"]
-- [("USD","EUR"),("EUR","USD")]
mkPairs :: [Currency] -> [Pair]
mkPairs currencies = [(f, t)  | f <- uniques, t <- uniques, f /= t]
  where
    uniques = nub currencies
