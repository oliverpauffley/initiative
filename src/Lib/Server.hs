module Lib.Server where

import Lib.App.Monad (AppEnv)
import Lib.Effects.Log (runAppAsHandler)
import Lib.Server.Auth (AuthRoutes, authServer)
import Lib.Server.Common (AppServer)
import Lib.Server.Game (GameRoutes (..), gameServer)
import Servant (Application, NamedRoutes, serve, (:-), (:>))
import Servant.API.Generic (ToServantApi, toServant)
import Servant.Server (Server, hoistServer)

type ToApi (site :: Type -> Type) = ToServantApi site

-- Server type level api
type Api = ToApi InitiativeApi

data InitiativeApi route = InitiativeApi
    { games :: route :- "games" :> NamedRoutes GameRoutes
    , auth :: route :- "auth" :> NamedRoutes AuthRoutes
    }
    deriving stock (Generic)

server :: AppEnv -> Server Api
server env =
    hoistServer
        (Proxy @Api)
        (runAppAsHandler env)
        (toServant apiServer)

application :: AppEnv -> Application
application env =
    serve
        (Proxy @Api)
        (server env)

apiServer :: InitiativeApi AppServer
apiServer =
    InitiativeApi
        { games = gameServer
        , auth = authServer
        }
