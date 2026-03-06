-- | Endpoints for creating and editing games.
module Lib.Server.Game where

import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (..))
import Lib.Db (getGameWithSessions, insertGame)
import Lib.Db.Game (getGamesWithSessions)
import Lib.Server.Common (AppServer, WithAuth, requireSession)
import Servant (Capture, Get, Header, JSON, Post, ReqBody, (:-), (:>))

data GameRoutes route = GameRoutes
    { getAllGames ::
        route :- Header "X-Session-Token" Text :> Get '[JSON] [Game]
    , getGame ::
        route :- Header "X-Session-Token" Text :> Capture "gameID" Int :> Get '[JSON] Game
    , postNewGame ::
        route
            :- Header "X-Session-Token" Text
                :> ReqBody '[JSON] NewGameRequest
                :> Post '[JSON] Game
    }
    deriving (Generic)

getAllGamesHandler :: (WithAuth env m) => Maybe Text -> m [Game]
getAllGamesHandler mHeader = requireSession mHeader *> getGamesWithSessions

getGameHandler :: (WithAuth env m) => Maybe Text -> Int -> m Game
getGameHandler mHeader gID = requireSession mHeader *> getGameWithSessions (GameID gID)

postNewGameHandler :: (WithAuth env m) => Maybe Text -> NewGameRequest -> m Game
postNewGameHandler mHeader r'@NewGameRequest{..} = do
    _ <- requireSession mHeader
    gID <- insertGame r'
    return $ Game gID newGameName newGameSystem []

gameServer :: GameRoutes AppServer
gameServer =
    GameRoutes
        { getAllGames = getAllGamesHandler
        , getGame = getGameHandler
        , postNewGame = postNewGameHandler
        }
