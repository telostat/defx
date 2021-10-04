module Defx.Config where

import           Control.Monad          (when)
import           Control.Monad.Except   (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson             ((.:))
import qualified Data.Aeson             as Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.Yaml              as Yaml
import           Defx.Providers.Oxr     (OxrConfig)
import qualified Path                   as P
import qualified Path.IO                as PIO


-- | Type encoding for DEFX program configuration.
data Config = Config
  { configDescription :: !T.Text
  , configProfiles    :: !(HM.HashMap T.Text Profile)
  } deriving Show


-- | 'Aeson.FromJSON' instance for 'Config'.
--
-- >>> Aeson.decode "{\"description\": \"Example\", \"profiles\": {}}" :: Maybe Config
-- Just (Config {configDescription = "Example", configProfiles = fromList []})
--
-- >>> Aeson.eitherDecode "{\"description\": \"Example\", \"profiles\": {\"example\": {\"provider\": \"oxr\"}}}" :: Either String Config
-- Left "Error in $.profiles.example: key \"apikey\" not found"
--
-- >>> Aeson.eitherDecode "{\"description\": \"Example\", \"profiles\": {\"example\": {\"provider\": \"oxr\", \"apikey\": \"<APIKEY>\"}}}" :: Either String Config
-- Right (Config {configDescription = "Example", configProfiles = fromList [("example",Profile {profileName = "example", profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<APIKEY>", oxrConfigCurrency = "USD"})})]})
--
-- >>> Aeson.eitherDecode "{\"description\": \"Example\", \"profiles\": {\"example\": {\"provider\": \"oxr\", \"apikey\": \"<APIKEY>\", \"base\": \"EUR\"}}}" :: Either String Config
-- Right (Config {configDescription = "Example", configProfiles = fromList [("example",Profile {profileName = "example", profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<APIKEY>", oxrConfigCurrency = "EUR"})})]})
instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o -> Config
    <$> o .: "description"
    <*> (HM.fromList . fmap (\(k, v) -> (k, Profile k v)) . HM.toList <$> o .: "profiles")


-- | Type encoding for configuration profiles.
data Profile = Profile
  { profileName     :: !T.Text
  , profileProvider :: !ProviderConfig
  } deriving Show


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
-- Right (Config {configDescription = "This is a configuration template for DEFX programs.\nYou can use it to derive your own configurations.\n", configProfiles = fromList [("example-oxr",Profile {profileName = "example-oxr", profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<CHANGE-ME>", oxrConfigCurrency = "USD"})})]})
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
-- >>> runExceptT (readConfigFileWithProfile $(mkRelFile "non-existing-file") "non-existing-profile") :: IO (Either String (Config, Profile))
-- Left "File does not exist: non-existing-file"
-- >>> runExceptT (readConfigFileWithProfile $(mkRelFile "config.tmpl.yaml") "non-existing-profile") :: IO (Either String (Config, Profile))
-- Left "Profile not found: non-existing-profile"
-- >>> runExceptT (readConfigFileWithProfile $(mkRelFile "config.tmpl.yaml") "example-oxr") :: IO (Either String (Config, Profile))
-- Right (Config {configDescription = "This is a configuration template for DEFX programs.\nYou can use it to derive your own configurations.\n", configProfiles = fromList [("example-oxr",Profile {profileName = "example-oxr", profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<CHANGE-ME>", oxrConfigCurrency = "USD"})})]},Profile {profileName = "example-oxr", profileProvider = ProviderConfigOxr (OxrConfig {oxrConfigApiKey = "<CHANGE-ME>", oxrConfigCurrency = "USD"})})
readConfigFileWithProfile :: (MonadError String m, MonadIO m) => P.Path a P.File -> T.Text -> m (Config, Profile)
readConfigFileWithProfile path profile = do
  config <- readConfigFile path
  case HM.lookup profile (configProfiles config) of
    Nothing -> throwError ("Profile not found: " <> T.unpack profile)
    Just pc -> pure (config, pc)
