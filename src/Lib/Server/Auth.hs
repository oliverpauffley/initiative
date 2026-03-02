-- | Endpoints for managing user auth.
module Lib.Server.Auth where

import Lib.Server.Common (AppServer)
import Servant (Get, JSON, (:-), (:>))

data AuthRoutes route = AuthRoutes
    { login :: route :- "login" :> Get '[JSON] Int
    , logout :: route :- "logout" :> Get '[JSON] Int
    }
    deriving stock (Generic)

loginHandler = return 1

logoutHandler = return 1

authServer :: AuthRoutes AppServer
authServer =
    AuthRoutes
        { login = loginHandler
        , logout = logoutHandler
        }
