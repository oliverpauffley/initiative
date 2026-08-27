{-# LANGUAGE OverloadedRecordDot #-}

module Lib where

import Lib.App (AppEnv, Env (..))
import Lib.App.Env (grab)
import Lib.Config (Config (..), loadConfig)
import Lib.Db (initialisePool)
import Lib.Effects.Log (mainLogAction)
import Lib.Server (application)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey)
import URI.ByteString (Absolute, Port (Port, portNumber), URIRef, parseURI, strictURIParserOptions)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    envDbPool <- initialisePool cDbCredentials
    envHttpManager <- newManager tlsManagerSettings
    key <- generateKey
    let envLogAction = mainLogAction cLogSeverity
        envPort = Port cPort
        envJWTSettings = defaultJWTSettings key
        envCookieSettings = defaultCookieSettings
        envGoogleClientID = cGoogleClientID
    pure Env{..}

parseUri :: Text -> URIRef Absolute
parseUri t = case parseURI strictURIParserOptions (encodeUtf8 t) of
    Right u -> u
    Left e -> error $ "Bad URI in config: " <> show e

runServer :: AppEnv -> IO ()
runServer env = run ((grab @Port) env).portNumber $ application env

main :: IO ()
main = loadConfig "config.toml" >>= mkAppEnv >>= runServer
