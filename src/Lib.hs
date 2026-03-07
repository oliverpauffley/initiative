{-# LANGUAGE OverloadedRecordDot #-}

module Lib where

import Lib.App (AppEnv, Env (..))
import Lib.App.Env (grab)
import Lib.Config (Config (..), OAuthConfig (..), loadConfig)
import Lib.Db (initialisePool)
import Lib.Effects.Log (mainLogAction)
import Lib.Server (application)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.OAuth.OAuth2 (OAuth2 (..))
import Network.Wai.Handler.Warp (run)
import URI.ByteString (Absolute, Port (Port, portNumber), URIRef, parseURI, strictURIParserOptions)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    envDbPool <- initialisePool cDbCredentials
    envHttpManager <- newManager tlsManagerSettings
    let envLogAction = mainLogAction cLogSeverity
        envOAuth = mkGoogleOAuth cOauthConfig
        envUserInfoUri = parseUri (oUserInfoUrl cOauthConfig)
        envPort = Port cPort
    pure Env{..}

parseUri :: Text -> URIRef Absolute
parseUri t = case parseURI strictURIParserOptions (encodeUtf8 t) of
    Right u -> u
    Left e -> error $ "Bad URI in config: " <> show e

mkGoogleOAuth :: OAuthConfig -> OAuth2
mkGoogleOAuth OAuthConfig{..} =
    OAuth2
        { oauth2ClientId = oId
        , oauth2ClientSecret = oSecret
        , oauth2AuthorizeEndpoint = parseUri "https://accounts.google.com/o/oauth2/v2/auth"
        , oauth2TokenEndpoint = parseUri "https://oauth2.googleapis.com/token"
        , oauth2RedirectUri = parseUri oCallbackUrl
        }

runServer :: AppEnv -> IO ()
runServer env = run ((grab @Port) env).portNumber $ application env

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
