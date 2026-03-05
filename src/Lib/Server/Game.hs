-- | Endpoints for creating and editing games.
module Lib.Server.Game where

import Lib.App (WithError)
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (..))
import Lib.Db (WithDb, getGameWithSessions, insertGame)
import Lib.Db.Game (getGamesWithSessions)
import Lib.Effects.Log (WithLog)
import Lib.Server.Common (AppServer)
import Servant (Capture, Get, JSON, Post, ReqBody, (:-), (:>))

-- TODO all need to be behind auth.
data GameRoutes route = GameRoutes
    { getAllGames ::
        route :- Get '[JSON] [Game]
    , getGame :: route :- Capture "gameID" Int :> Get '[JSON] Game
    , postNewGame ::
        route
            :- ReqBody '[JSON] NewGameRequest
                :> Post '[JSON] Game
    }
    deriving (Generic)

getAllGamesHandler :: (WithDb env m, WithError m, WithLog env m) => m [Game]
getAllGamesHandler = do
    getGamesWithSessions

getGameHandler :: (WithDb env m, WithError m, WithLog env m) => Int -> m Game
getGameHandler gID = getGameWithSessions (GameID gID)

postNewGameHandler :: (WithDb env m, WithError m, WithLog env m) => NewGameRequest -> m Game
postNewGameHandler r'@NewGameRequest{..} = do
    gID <- insertGame r'
    return $ Game gID newGameName newGameSystem []

gameServer :: GameRoutes AppServer
gameServer =
    GameRoutes
        { getAllGames = getAllGamesHandler
        , getGame = getGameHandler
        , postNewGame = postNewGameHandler
        }
