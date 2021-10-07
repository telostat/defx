module Defx.IO where

import           Control.Monad.Except   (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import           Defx.Types             (DailyRates)
import qualified Path                   as P
import qualified Path.IO                as PIO
import qualified System.IO              as IO


-- | Type alias for type-safe absolute file paths.
type AbsFilePath = P.Path P.Abs P.File


-- | Type alias for type-safe absolute directory paths.
type AbsDirPath = P.Path P.Abs P.Dir


-- | Type alias for type-safe relative file paths.
type RelFilePath = P.Path P.Rel P.File


-- | Type alias for type-safe relative directory paths.
type RelDirPath = P.Path P.Rel P.Dir


-- | Writes given daily rates to the given IO handle.
hPutDailyRates :: MonadIO m => DailyRates -> IO.Handle -> m ()
hPutDailyRates v h = liftIO (BL.hPut h (Aeson.encode v))


-- | Writes given daily rates to the file at the given path.
writeDailyRates :: MonadIO m => DailyRates -> FilePath -> m ()
writeDailyRates v p = liftIO (IO.withFile p IO.WriteMode (hPutDailyRates v))


-- | Attempts to convert a 'FilePath' to type-safe absolute file path,
-- optionally checking if file exists (TODO and is readable).
toAbsFilePath
  :: (MonadError String m, MonadIO m)
  => Bool
  -> FilePath
  -> m AbsFilePath
toAbsFilePath check path = do
  resolved <- PIO.resolveFile' path
  if check then checkFileExists resolved else pure resolved


-- | Guard that returns the given absolute file path if it exists, throws an
-- error otherwise.
checkFileExists :: (MonadError String m, MonadIO m) => AbsFilePath -> m AbsFilePath
checkFileExists path = do
  check <- PIO.doesFileExist path
  if check then pure path else throwError ("File does not exist: " <> P.toFilePath path)


parseRelFileE :: MonadError String m => String -> m RelFilePath
parseRelFileE p = either (throwError . show) pure (P.parseRelFile p)


parseRelDirE :: MonadError String m => String -> m RelDirPath
parseRelDirE p = either (throwError . show) pure (P.parseRelDir p)
