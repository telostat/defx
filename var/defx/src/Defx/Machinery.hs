module Defx.Machinery where

import           Control.Monad.Except (MonadError(throwError))
import qualified Data.HashMap.Strict  as HM
import           Data.List            (nub)
import           Data.Maybe           (fromMaybe)
import           Defx.Types           (Currency, FXRate(..), Pair, Rates, Value(..))
import           Numeric.Decimal      (divideDecimalWithRounding)


-- | Computes FX rates for a given list of currency pairs from a given
-- repository of rates.
computeCrosses
  :: MonadError String m
  => Rates   -- ^ The repository of rates to compute rates from.
  -> [Pair]  -- ^ The list of currency pairs to compute rates for.
  -> m [FXRate]
computeCrosses rates = mapM (computeCross rates)


-- | Compute the FX rate for a given currency pair from a given repository of
-- rates.
computeCross
  :: MonadError String m
  => Rates  -- ^ The repository of rates to compute rates from.
  -> Pair   -- ^ The currency pair to compute rates for.
  -> m FXRate
computeCross rates pair@(ccy1, ccy2) = do
  rate1 <- findRate ccy1
  rate2 <- findRate ccy2
  let rate = divideValueWeird rate2 rate1
  pure (FXRate pair rate)
  where
    findRate ccy = maybe (throwError ("No rate found for currency: " <> show ccy)) pure (HM.lookup ccy rates)
    divideValueWeird (MkValue a) (MkValue b) = MkValue (fromMaybe 0 (divideDecimalWithRounding a b))


-- | Creates a list of pairs from a given list of currencies.
--
-- >>> mkPairs []
-- []
--
-- >>> mkPairs ["USD"]
-- []
--
-- >>> mkPairs ["USD", "EUR"]
-- [("USD","EUR"),("EUR","USD")]
mkPairs :: [Currency] -> [Pair]
mkPairs currencies =
  let
    uniques = nub currencies
  in
    [(f, t)  | f <- uniques, t <- uniques, f /= t]
