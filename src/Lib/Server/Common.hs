-- | Anything that needs to be shared between multiple sub components of the server package
module Lib.Server.Common (
    AppServer,
    WithAuth,
    requireAuth,
    requireSession,
    requireAdmin,
    requirePlayer,
) where

import Data.UUID (fromText)
import Lib.App (App)
import Lib.App.Error (WithError, headerDecodeError, missingHeader, notAllowed, throwError)
import Lib.Core.Player (Player (..), PlayerID)
import Lib.Core.UserSession (SessionToken (..), UserSession (..))
import Lib.Db (WithDb)
import Lib.Db.Player (getPlayerByID)
import Lib.Db.UserSession (lookupSession)
import Lib.Effects.Log (WithLog)
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Server.Generic (AsServerT)

type AppServer = AsServerT App

-- Group of some common monads that every endpoint (with auth) needs
type WithAuth env m = (WithDb env m, WithError m, WithLog env m)

requireAuth :: AuthResult UserSession -> (UserSession -> App a) -> App a
requireAuth (Authenticated token) handler = handler token
requireAuth _ _ = throwError $ notAllowed "Unauthorized access"

-- | Validate the X-Session-Token header and return the authenticated session.
requireSession :: (WithAuth env m) => Maybe Text -> m UserSession
requireSession Nothing = throwError $ missingHeader "X-Session-Token"
requireSession (Just raw) = do
    uuid <- case fromText raw of
        Nothing -> throwError $ headerDecodeError "X-Session-Token is not a valid UUID"
        Just u -> pure u
    lookupSession (SessionToken uuid)

-- | Like 'requireSession' but additionally asserts the caller is an admin.
requireAdmin :: (WithAuth env m) => Maybe Text -> m (Player, UserSession)
requireAdmin mRaw = do
    sess <- requireSession mRaw
    player <- getPlayerByID (sessionPlayerID sess)
    if playerIsAdmin player
        then pure (player, sess)
        else throwError $ notAllowed "Admin access required"

-- | Like 'requireSession' but require that player id is the same as the given ID
requirePlayer :: (WithAuth env m) => Maybe Text -> PlayerID -> m UserSession
requirePlayer mRaw playerID = do
    sess <- requireSession mRaw
    if sessionPlayerID sess == playerID
        then pure sess
        else throwError $ notAllowed "Cannot process request for different user"
