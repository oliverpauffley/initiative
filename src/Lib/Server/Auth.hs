{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Endpoints for managing user auth.
module Lib.Server.Auth where

import Control.Lens ((.~), (^.), (^?), _Just)
import Crypto.JOSE (FromCompact, HasParams, JWKSet, JWS, JWSHeader, decodeCompact)
import Crypto.JWT (ClaimsSet, HasAudiencePredicate (audiencePredicate), HasClaimsSet (claimSub, claimsSet), JWTError, JWTValidationSettings, SignedJWT, StringOrURI, defaultJWTValidationSettings, string, verifyJWT)

import Data.Aeson (FromJSON, ToJSON (..), object, parseJSON, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lib.App (GoogleClientID, Has, grab)
import Lib.App.Error (jwtError, notAllowed, serverError, throwError, throwOnNothing)
import Lib.Core.Player (Player (playerID))
import Lib.Db.Player (getPlayerBySub)
import Lib.Db.UserSession (createSession)
import Lib.Server.Common (AppServer, WithAuth)
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Servant (
    GenericMode (type (:-)),
    Header,
    Headers,
    JSON,
    NoContent (NoContent),
    ReqBody,
    StdMethod (POST),
    Verb,
    type (:>),
 )
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie, acceptLogin)

-- | Constraint for monadic actions for authentication
type WithAuth2 m = (MonadIO m)

data Login = Login
    { -- This is the google token which we will swap for a generated JWT token for the app
      token :: ByteString
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON Login where
    toJSON (Login tok) = object ["token" .= TE.decodeUtf8 tok]

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o ->
        Login . TE.encodeUtf8 <$> o .: "token"

data GoogleClaims = GoogleClaims
    { jwtClaims :: ClaimsSet
    , email :: Text
    }
    deriving (Generic, Show)

instance HasClaimsSet GoogleClaims where
    claimsSet f s = fmap (\a' -> s{jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON GoogleClaims
instance ToJSON GoogleClaims

-- TODO add new user add for admin (just seeds the email)
-- TODO add new user add for first login
data AuthRoutes route = AuthRoutes
    { login ::
        route
            :- "login"
                :> ReqBody '[JSON] Login
                :> Verb
                    'POST
                    204
                    '[JSON]
                    ( Headers
                        '[ Header "Set-Cookie" SetCookie
                         , Header "Set-Cookie" SetCookie
                         ]
                        NoContent
                    )
    }
    deriving (Generic)

-- | Run an ExceptT IO action, converting the Left case into a server error.
liftOAuth :: (WithAuth env m) => (e -> Text) -> Text -> ExceptT e IO a -> m a
liftOAuth showErr ctx action =
    liftIO (runExceptT action) >>= \case
        Left e -> throwError $ serverError $ ctx <> ": " <> showErr e
        Right a -> pure a

fetchCerts :: (WithAuth2 m) => m JWKSet
fetchCerts = do
    response <- httpJSON "https://www.googleapis.com/oauth2/v3/certs"
    return $ getResponseBody response

verifyGoogleToken :: (Has GoogleClientID env, WithAuth env m) => Login -> m GoogleClaims
verifyGoogleToken (Login tokenStr) = do
    clientID <- grab @GoogleClientID

    liftOAuth showJwtError "failed to verify Google JWT" $ do
        jwt <- decodeCompact (BL.fromStrict tokenStr) :: _ SignedJWT
        jwks <- fetchCerts

        verifyJWT (validationSettings clientID) jwks jwt
  where
    showJwtError :: JWTError -> Text
    showJwtError = T.pack . show

    validationSettings :: GoogleClientID -> JWTValidationSettings
    validationSettings cID =
        defaultJWTValidationSettings issuerCheck
            & audiencePredicate
            .~ audienceCheck (fromString . T.unpack $ cID)

    audienceCheck :: StringOrURI -> StringOrURI -> Bool
    audienceCheck clientID aud =
        aud == clientID

    issuerCheck :: StringOrURI -> Bool
    issuerCheck iss =
        iss == "https://accounts.google.com"
            || iss == "accounts.google.com"

loginHandler ::
    (Has GoogleClientID env, Has CookieSettings env, Has JWTSettings env, WithAuth env m) =>
    Login ->
    m
        ( Headers
            '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
            NoContent
        )
loginHandler t = do
    claims <- verifyGoogleToken t
    sub <- throwOnNothing (jwtError "failed to get sub") (claims ^. claimSub ^? _Just . string)
    player <- getPlayerBySub sub
    sToken <- createSession $ playerID player
    cSettings <- grab @CookieSettings
    jSettings <- grab @JWTSettings
    mApplyCookies <- liftIO $ acceptLogin cSettings jSettings sToken
    case mApplyCookies of
        Nothing -> throwError $ notAllowed "couldnt not authorize user"
        Just applyCookies -> pure $ applyCookies NoContent

authServer :: AuthRoutes AppServer
authServer =
    AuthRoutes
        { login = loginHandler
        }
