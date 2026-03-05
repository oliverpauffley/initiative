-- | Endpoints for managing user auth.
module Lib.Server.Auth where

import Lib.App.Error (WithError)
import Lib.Core.Player (Email (..), NewPlayerRequest, Player (..), PlayerID (..))
import Lib.Db (WithDb)
import Lib.Db.Player (getPlayer, insertPlayer)
import Lib.Effects.Log (WithLog)
import Lib.Server.Common (AppServer)
import Servant (Get, JSON, Post, ReqBody, (:-), (:>))

data AuthRoutes route = AuthRoutes
    { -- TODO needs to use oauth to get the email rather than just taking it from a requestBody
      login :: route :- "login" :> ReqBody '[JSON] Text :> Get '[JSON] PlayerID
    , logout :: route :- "logout" :> Get '[JSON] Int
    , -- TODO Needs to be an admin only endpoint I think?
      addNewUser :: route :- "player" :> ReqBody '[JSON] NewPlayerRequest :> Post '[JSON] PlayerID
    }
    deriving stock (Generic)

loginHandler :: (WithDb env m, WithError m, WithLog env m) => Text -> m PlayerID
loginHandler email = do
    Player{..} <- getPlayer $ Email email
    return playerID

logoutHandler :: (WithDb env m, WithError m, WithLog env m) => m Int
logoutHandler = return 1

addNewUserHandler :: (WithDb env m, WithError m, WithLog env m) => NewPlayerRequest -> m PlayerID
addNewUserHandler = insertPlayer

authServer :: AuthRoutes AppServer
authServer =
    AuthRoutes
        { login = loginHandler
        , logout = logoutHandler
        , addNewUser = addNewUserHandler
        }
