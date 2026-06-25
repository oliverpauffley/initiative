-- | Endpoints for creating and editing games.
module Lib.Server.Game where

import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (..))
import Lib.Core.UserSession (UserSession)
import Lib.Db (getGameWithSessions, insertGame)
import Lib.Db.Game (getGamesWithSessions)
import Lib.Server.Common (AppServer, WithAuth, requireAuth)
import Servant (Capture, Get, JSON, Post, ReqBody, (:-), (:>))
import Servant.Auth.Server (AuthResult)

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

getAllGamesHandler :: (WithAuth env m) => m [Game]
getAllGamesHandler = getGamesWithSessions

getGameHandler :: (WithAuth env m) => Int -> m Game
getGameHandler gID = getGameWithSessions (GameID gID)

-- TODO require admin
postNewGameHandler :: (WithAuth env m) => NewGameRequest -> m Game
postNewGameHandler r'@NewGameRequest{..} = do
    gID <- insertGame r'
    return $ Game gID newGamePlayerID newGameName newGameSystem []

gameServer :: AuthResult UserSession -> GameRoutes AppServer
gameServer authResult =
    GameRoutes
        { getAllGames = requireAuth authResult $ const getAllGamesHandler
        , getGame = \gameID -> requireAuth authResult $ \_ -> getGameHandler gameID
        , postNewGame = \gameReq -> requireAuth authResult $ \_ -> postNewGameHandler gameReq
        }
