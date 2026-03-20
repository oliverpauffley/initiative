module Lib.Db.Game (getGamesWithSessions, insertGame, getGameWithSessions) where

import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import Lib.App (WithError)
import Lib.Core.Game (
    Game (Game, gameSessions),
    GameID (..),
    NewGameRequest (..),
    Session (Session),
 )
import Lib.Core.Interval (Interval (..))
import Lib.Core.Player (PlayerID)
import Lib.Db.Functions (WithDb, asSingleRow, query, queryWith, queryWith_)

getGamesWithSessions :: (WithDb env m, WithError m) => m [Game]
getGamesWithSessions = do
    let sql =
            "SELECT g.id, g.game_master_id, g.name, g.system, s.start_time, s.end_time, s.name \
            \FROM games g \
            \LEFT JOIN sessions s ON g.id = s.game_id"

    flatRows <- queryWith_ parseGameJoinRow sql

    return $ buildGames flatRows

getGameWithSessions :: (WithDb env m, WithError m) => GameID -> m Game
getGameWithSessions gameId = do
    let sql =
            "SELECT g.id, g.game_master_id, g.name, g.system, s.start_time, s.end_time, s.name \
            \FROM games g \
            \LEFT JOIN sessions s ON g.id = s.game_id \
            \WHERE g.id = ?"

    asSingleRow $ buildGames <$> queryWith parseGameJoinRow sql (Only gameId)

-- Parse a row of the query
parseGameJoinRow :: RowParser (GameID, PlayerID, Text, Text, Maybe Session)
parseGameJoinRow = do
    gId <- field
    gPId <- field
    gName <- field
    gSys <- field

    mStart <- field :: RowParser (Maybe UTCTime)
    mEnd <- field
    sName <- field

    let mSession = do
            start <- mStart
            end <- mEnd
            return $ Session (Interval start end) sName

    return (gId, gPId, gName, gSys, mSession)

buildGames :: [(GameID, PlayerID, Text, Text, Maybe Session)] -> [Game]
buildGames rows = Map.elems (foldr step Map.empty rows)
  where
    step (gId, gPID, gName, gSys, mSession) acc =
        let
            newSessions = maybe [] pure mSession
            newGame = Game gId gPID gName gSys newSessions
            combineGames _new old = old{gameSessions = gameSessions old ++ newSessions}
         in
            Map.insertWith combineGames gId newGame acc

-- | Insert a new game into the database. Returns the generated ID.
insertGame :: (WithDb env m, WithError m) => NewGameRequest -> m GameID
insertGame req = do
    let sql = "INSERT INTO games (name, system, game_master_id) VALUES (?,?,?) RETURNING id;"
    asSingleRow $ query sql req
