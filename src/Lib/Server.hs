{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib.Server where

import Colog (log, pattern D, pattern I)
import Data.Aeson (FromJSON)
import Lib.App (WithError)
import Lib.App.Monad (App, AppEnv)
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (..))
import Lib.Db (WithDb)
import Lib.Db.Game (getGamesWithSessions, insertGame)
import Lib.Effects.Log (WithLog, runAppAsHandler)
import Servant (Application, Capture, Get, JSON, Post, ReqBody, serve, (:-), (:<|>), (:>))
import Servant.API.Generic (ToServantApi, toServant)
import Servant.Server (Server, hoistServer)
import Servant.Server.Generic (AsServerT)

type ToApi (site :: Type -> Type) = ToServantApi site

-- Server type level api
type Api = ToApi Site
type AppServer = AsServerT App

data Site route = Site
    { getAllGames ::
        route
            :- "games" :> Get '[JSON] [Game]
    , postNewGame ::
        route
            :- "games" :> ReqBody '[JSON] NewGameRequest :> Post '[JSON] Game
    }
    deriving (Generic)

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
        { getAllGames = getAllGamesHandler
        , postNewGame = postNewGameHandler
        }

getAllGamesHandler :: (WithDb env m, WithError m, WithLog env m) => m [Game]
getAllGamesHandler = do
    getGamesWithSessions

postNewGameHandler :: (WithDb env m, WithError m, WithLog env m) => NewGameRequest -> m Game
postNewGameHandler r'@NewGameRequest{..} = do
    gID <- insertGame r'
    return $ Game gID newGameName newGameSystem []
