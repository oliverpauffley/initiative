-- | Configuration through the @config.toml@ file@.
module Lib.Config (Config (..), OAuthConfig (..), loadConfig) where

import Toml (TomlCodec, diwrap, table, (.=))

import Colog (Severity)
import qualified Toml

-- | Config for google oauth
data OAuthConfig = OAuthConfig
    { oName :: !Text
    , oId :: !Text
    , oSecret :: !Text
    , oCallbackUrl :: !Text
    }

-- | Data type for the configurable elements of the application
data Config = Config
    { cDbCredentials :: !ByteString
    , cLogSeverity :: !Severity
    , cOauthConfig :: !OAuthConfig
    }

oauthConfigCodec :: TomlCodec OAuthConfig
oauthConfigCodec =
    OAuthConfig
        <$> Toml.diwrap (Toml.text "name") .= oName
        <*> Toml.diwrap (Toml.text "id") .= oId
        <*> Toml.diwrap (Toml.text "secret") .= oSecret
        <*> Toml.diwrap (Toml.text "callback_url") .= oCallbackUrl

-- | TOML codec for the 'Config' data type.
configT :: TomlCodec Config
configT =
    Config
        <$> Toml.byteString "dbCredentials" .= cDbCredentials
        <*> Toml.read "log.severity" .= cLogSeverity
        <*> Toml.table oauthConfigCodec "oauth-google" .= cOauthConfig

-- | Loads the @config.toml@ file.
loadConfig :: (MonadIO m) => m Config
loadConfig = Toml.decodeFile configT "config.toml"
