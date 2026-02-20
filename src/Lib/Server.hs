{-# LANGUAGE PatternSynonyms #-}

module Lib.Server where

import Colog (log, pattern D)
import Lib.App (WithError)
import Lib.App.Monad (App, AppEnv)
import Lib.Db (WithDb)
import Lib.Effects.Log (WithLog, runAppAsHandler)
import Servant (Application, Capture, Get, JSON, ReqBody, serve, (:-), (:>))
import Servant.API.Generic (ToServantApi, toServant)
import Servant.Server (Server, hoistServer)
import Servant.Server.Generic (AsServerT)

type ToApi (site :: Type -> Type) = ToServantApi site

-- Server type level api
type Api = ToApi Site
type AppServer = AsServerT App

data Site route = Site
    { getRoute ::
        route
            :- "events" :> Get '[JSON] GetResponse
    }
    deriving (Generic)

type GetResponse = Text

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

apiServer :: Site AppServer
apiServer =
    Site
        { getRoute = getHandler
        }

getHandler :: (WithDb env m, WithError m, WithLog env m) => m GetResponse
getHandler = do
    log D "test debug"
    return "hello!"
