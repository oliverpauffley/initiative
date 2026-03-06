-- | Endpoints for managing user auth.
module Lib.Server.Auth where

import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Aeson
import Lib.App.Env (Has, grab)
import Lib.App.Error (invalid, redirect, serverError, throwError)
import Lib.Core.Player (Email (..), NewPlayerRequest, Player (..), PlayerID)
import Lib.Core.UserSession (SessionToken, UserSession (..))
import Lib.Db.Player (getPlayer, insertPlayer)
import Lib.Db.UserSession (createSession, deleteSession)
import Lib.Server.Common (AppServer, WithAuth, requireAdmin, requireSession)
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (
    ExchangeToken (..),
    OAuth2,
    OAuth2Token (..),
    authorizationUrlWithParams,
 )
import Network.OAuth.OAuth2.HttpClient (authGetJSON)
import Network.OAuth.OAuth2.TokenRequest (fetchAccessToken)
import Servant hiding (throwError)
import URI.ByteString (Absolute, URIRef, serializeURIRef')

-- | Subset of Google userinfo response
newtype GoogleUser = GoogleUser {googleEmail :: Text}

instance FromJSON GoogleUser where
    parseJSON = Aeson.withObject "GoogleUser" $ \o ->
        GoogleUser <$> o .: "email"

data AuthRoutes route = AuthRoutes
    { googleLogin ::
        route :- "google" :> "login" :> Get '[JSON] NoContent
    , googleComplete ::
        route
            :- "google"
                :> "complete"
                :> QueryParam "code" Text
                :> Get '[JSON] SessionToken
    , logout ::
        route
            :- "logout"
                :> Header "X-Session-Token" Text
                :> Post '[JSON] NoContent
    , addNewUser ::
        route
            :- "player"
                :> Header "X-Session-Token" Text
                :> ReqBody '[JSON] NewPlayerRequest
                :> Post '[JSON] PlayerID
    }
    deriving stock (Generic)

googleLoginHandler ::
    ( WithAuth env m
    , Has OAuth2 env
    ) =>
    m NoContent
googleLoginHandler = do
    oauth <- grab @OAuth2
    let uri = authorizationUrlWithParams [("scope", "openid email profile")] oauth
        uriText = decodeUtf8 $ serializeURIRef' uri
    throwError $ redirect uriText

googleCompleteHandler ::
    ( WithAuth env m
    , Has OAuth2 env
    , Has Manager env
    , Has (URIRef Absolute) env
    ) =>
    Maybe Text ->
    m SessionToken
googleCompleteHandler Nothing = throwError $ invalid "Missing 'code' query parameter"
googleCompleteHandler (Just code) = do
    oauth <- grab @OAuth2
    mgr <- grab @Manager
    userInfoUri <- grab @(URIRef Absolute)
    oauthToken <- liftOAuth show "Token exchange failed" $
        fetchAccessToken mgr oauth (ExchangeToken code)
    GoogleUser{..} <- liftOAuth decodeUtf8 "Userinfo request failed" $
        authGetJSON mgr (accessToken oauthToken) userInfoUri
    player <- getPlayer (Email googleEmail)
    createSession (playerID player)

-- | Run an ExceptT IO action, converting the Left case into a server error.
liftOAuth :: (WithAuth env m) => (e -> Text) -> Text -> ExceptT e IO a -> m a
liftOAuth showErr ctx action =
    liftIO (runExceptT action) >>= \case
        Left e -> throwError $ serverError $ ctx <> ": " <> showErr e
        Right a -> pure a

logoutHandler ::
    (WithAuth env m) =>
    Maybe Text ->
    m NoContent
logoutHandler mHeader = do
    sess <- requireSession mHeader
    deleteSession (sessionToken sess)
    pure NoContent

addNewUserHandler ::
    (WithAuth env m) =>
    Maybe Text ->
    NewPlayerRequest ->
    m PlayerID
addNewUserHandler mHeader req = do
    _ <- requireAdmin mHeader
    insertPlayer req

authServer :: AuthRoutes AppServer
authServer =
    AuthRoutes
        { googleLogin = googleLoginHandler
        , googleComplete = googleCompleteHandler
        , logout = logoutHandler
        , addNewUser = addNewUserHandler
        }
