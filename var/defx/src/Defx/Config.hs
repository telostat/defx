{-# LANGUAGE TemplateHaskell #-}

module Defx.Config where

import           Control.Monad          (when)
import           Control.Monad.Except   (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson             ((.:))
import qualified Data.Aeson             as Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Data.Time              (Day)
import qualified Data.Yaml              as Yaml
import           Defx.IO                (AbsDirPath, AbsFilePath, parseRelFileE)
import           Defx.Providers.Oxr     (OxrConfig)
import           Defx.Types             (Currency)
import           GHC.Base               (NonEmpty)
import           Path                   (Dir, Rel, mkRelDir, (</>))
import qualified Path                   as P
import qualified Path.IO                as PIO
import           Text.Printf            (printf)


-- | Type encoding for DEFX program configuration.
data Config = Config
  { configDescription :: !T.Text
  , configProfiles    :: !(HM.HashMap T.Text Profile)
  } deriving Show


-- | 'Aeson.FromJSON' instance for 'Config'.
--
-- >>> Aeson.decode "{\"description\": \"Example\", \"profiles\": []}" :: Maybe Config
-- Just (Config {configDescription = "Example", configProfiles = fromList []})
--
-- >>> Aeson.eitherDecode "{\"description\": \"Example\", \"profiles\": [{\"name\": \"example\", \"directory\": \"/data/defx/example\", \"currencies\": [\"USD\", \"CHF\", \"EUR\"], \"provider\": \"oxr\", \"apikey\": \"<APIKEY>\"}]}" :: Either String Config
-- Right (Config {configDescription = "Example", configProfiles = fromList [("example",Profile {profileName = "example", profileDirectory = "/data/defx/example/", profileCurrencies = "USD" :| ["CHF","EUR"], profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<APIKEY>", oxrConfigCurrency = "USD"})})]})
instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o -> Config
    <$> o .: "description"
    <*> (HM.fromList . fmap (\v -> (profileName v, v)) <$> o .: "profiles")


-- | Type encoding for configuration profiles.
data Profile = Profile
  { profileName       :: !T.Text
  , profileDirectory  :: !AbsDirPath
  , profileCurrencies :: !(NonEmpty Currency)
  , profileProvider   :: !ProviderConfig
  } deriving Show


-- | 'Aeson.FromJSON' instance for 'Profile'.
instance Aeson.FromJSON Profile where
  parseJSON = Aeson.withObject "Profile" $ \o -> Profile
    <$> o .: "name"
    <*> o .: "directory"
    <*> o .: "currencies"
    <*> Aeson.parseJSON (Aeson.Object o)


-- | Type encoding for provider configurations.
newtype ProviderConfig = ProviderConfigOxr OxrConfig
  deriving Show


-- | 'Aeson.FromJSON' instance for 'ProviderConfig'.
--
-- >>> Aeson.eitherDecode "{\"provider\": \"oxr\", \"apikey\": \"<APIKEY>\"}" :: Either String ProviderConfig
-- Right (ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<APIKEY>", oxrConfigCurrency = "USD"}))
--
-- >>> Aeson.eitherDecode "{\"provider\": \"oxr\", \"apikey\": \"<APIKEY>\", \"base\": \"EUR\"}" :: Either String ProviderConfig
-- Right (ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<APIKEY>", oxrConfigCurrency = "EUR"}))
instance Aeson.FromJSON ProviderConfig where
  parseJSON = Aeson.withObject "ProviderConfig" $ \o -> do
    provider <- o .: "provider"
    case T.toUpper provider of
      "OXR" -> ProviderConfigOxr <$> Aeson.parseJSON (Aeson.Object o)
      _     -> fail ("Unknown provider: " <> show provider)


-- | Reads a configuration file, parses the configuration and returns it.
--
-- >>> import Control.Monad.Except (runExceptT)
-- >>> import Path
-- >>> :set -XTemplateHaskell
-- >>> runExceptT (readConfigFile $(mkRelFile "non-existing-file")) :: IO (Either String Config)
-- Left "File does not exist: non-existing-file"
-- >>> runExceptT (readConfigFile $(mkRelFile "config.tmpl.yaml")) :: IO (Either String Config)
-- Right (Config {configDescription = "This is a configuration template for DEFX programs.\nYou can use it to derive your own configurations.\n", configProfiles = fromList [("example",Profile {profileName = "example", profileDirectory = "/data/defx/oxr/", profileCurrencies = "USD" :| ["CHF","EUR"], profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<CHANGE-ME>", oxrConfigCurrency = "USD"})})]})
readConfigFile :: (MonadError String m, MonadIO m) => P.Path a P.File -> m Config
readConfigFile path = do
  PIO.doesFileExist path >>= flip when (throwError ("File does not exist: " <> P.toFilePath path)) . not
  decodeAttempt path >>= either (throwError . show) pure
  where
    decodeAttempt = liftIO . Yaml.decodeFileEither . P.toFilePath


-- | Reads a configuration file, parses the configuration and returns it along
-- with the requested profile.
--
-- >>> import Control.Monad.Except (runExceptT)
-- >>> import Path
-- >>> :set -XTemplateHaskell
-- >>> runExceptT (readConfigFileWithProfile $(mkRelFile "non-existing-file") "some-profile") :: IO (Either String (Config, Profile))
-- Left "File does not exist: non-existing-file"
-- >>> runExceptT (readConfigFileWithProfile $(mkRelFile "config.tmpl.yaml") "non-existing-profile") :: IO (Either String (Config, Profile))
-- Left "Profile not found: non-existing-profile"
-- >>> runExceptT (readConfigFileWithProfile $(mkRelFile "config.tmpl.yaml") "example") :: IO (Either String (Config, Profile))
-- Right (Config {configDescription = "This is a configuration template for DEFX programs.\nYou can use it to derive your own configurations.\n", configProfiles = fromList [("example",Profile {profileName = "example", profileDirectory = "/data/defx/oxr/", profileCurrencies = "USD" :| ["CHF","EUR"], profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<CHANGE-ME>", oxrConfigCurrency = "USD"})})]},Profile {profileName = "example", profileDirectory = "/data/defx/oxr/", profileCurrencies = "USD" :| ["CHF","EUR"], profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<CHANGE-ME>", oxrConfigCurrency = "USD"})})
readConfigFileWithProfile :: (MonadError String m, MonadIO m) => P.Path a P.File -> T.Text -> m (Config, Profile)
readConfigFileWithProfile path profile = do
  config <- readConfigFile path
  case HM.lookup profile (configProfiles config) of
    Nothing -> throwError ("Profile not found: " <> T.unpack profile)
    Just pc -> pure (config, pc)


-- | Returns the daily data directory for the given profile.
profileDailyDataDirectory :: Profile -> AbsDirPath
profileDailyDataDirectory profile = profileDirectory profile </> $(mkRelDir "daily")


-- | Returns the daily data file for the given profile and given date.
profileDailyDataFile :: MonadError String m => Profile -> Day -> m AbsFilePath
profileDailyDataFile profile date = (profileDailyDataDirectory profile </>) <$> parseRelFileE (printf "%s.json" (show date))


ensureProfileDailyDataDirectory :: (MonadError String m, MonadIO m) => Profile -> m AbsDirPath
ensureProfileDailyDataDirectory profile = do
  let path = profileDailyDataDirectory profile
  PIO.createDirIfMissing True path
  pure path
