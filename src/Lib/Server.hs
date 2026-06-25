{-# LANGUAGE TypeApplications #-}

module Lib.Server where

import Lib.App (App, Env (..))
import Lib.App.Monad (AppEnv)
import Lib.Core.UserSession (UserSession)
import Lib.Effects.Log (runAppAsHandler)
import Lib.Server.Auth (AuthRoutes, authServer)
import Lib.Server.Availability (AvailabilityRoutes (..), availabilityServer)
import Lib.Server.Game (GameRoutes (..), gameServer)
import Servant (Application, Context (EmptyContext, (:.)), HasServer (hoistServerWithContext), NamedRoutes, serveWithContext, (:-), (:>))
import Servant.API.Generic (ToServantApi, toServant)
import Servant.Auth.Server (Auth, AuthResult, CookieSettings, JWT, JWTSettings)
import Servant.Server (Server)
import Servant.Server.Generic (AsServerT)

type ToApi (site :: Type -> Type) = ToServantApi site

data Unprotected route = AuthRoutes

data Protected route = Protected
    { games :: route :- "games" :> NamedRoutes GameRoutes
    , availability :: route :- "availablilty" :> NamedRoutes AvailabilityRoutes
    }
    deriving stock (Generic)

data Site auths route = Site
    { unprotected :: route :- NamedRoutes AuthRoutes
    , protected :: route :- Auth auths UserSession :> NamedRoutes Protected
    }
    deriving (Generic)

type Api auths = ToServantApi (Site auths)
type AuthContext = '[CookieSettings, JWTSettings]

server :: AppEnv -> Server (Api '[JWT])
server env =
    let
        contextProxy = Proxy @AuthContext
     in
        hoistServerWithContext
            (Proxy @(Api '[JWT]))
            contextProxy
            (runAppAsHandler env)
            (toServant apiServer)

application :: AppEnv -> Application
application env =
    let
        cSettings = envCookieSettings env
        jSettings = envJWTSettings env
        context = cSettings :. jSettings :. EmptyContext
     in
        serveWithContext
            (Proxy @(Api '[JWT]))
            context
            (server env)

protectedServer :: AuthResult UserSession -> Protected (AsServerT App)
protectedServer authResult =
    Protected
        { games = gameServer authResult
        , availability = availabilityServer authResult
        }

apiServer :: Site auths (AsServerT App)
apiServer =
    Site
        { unprotected = authServer
        , protected = protectedServer
        }
