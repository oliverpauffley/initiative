-- | Configuration through the @config.toml@ file@.
module Lib.Config (Config (..), loadConfig) where

import Toml (TomlCodec, (.=))

import Colog (Severity)
import qualified Toml

-- | Data type for the configurable elements of the application
data Config = Config
    { cDbCredentials :: !ByteString
    , cLogSeverity :: !Severity
    , -- port for running the web server on.
      cPort :: !Int
    , -- the google client ID
      cGoogleClientID :: !Text
    }

-- | TOML codec for the 'Config' data type.
configT :: TomlCodec Config
configT =
    Config
        <$> Toml.byteString "dbCredentials" .= cDbCredentials
        <*> Toml.read "log.severity" .= cLogSeverity
        <*> Toml.int "port" .= cPort
        <*> Toml.text "googleClientID" .= cGoogleClientID

-- | Loads the @config.toml@ file.
loadConfig :: (MonadIO m) => m Config
loadConfig = Toml.decodeFile configT "config.toml"
