module Lib.Db.Availability where

import Control.Lens (over)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import Lib.App.Error (WithError, throwError)
import Lib.Core.Availability (Availability (..), addAvailability)
import Lib.Core.Game (GameID (..))
import Lib.Core.Interval (Interval (..))
import Lib.Core.Player (PlayerID)
import Lib.Db.Functions (WithDb, executeMany, queryWith, singleRowError)

insertAvailability :: (WithDb env m, WithError m) => Availability -> m ()
insertAvailability req = do
    let sql =
            "INSERT INTO availability (game_id, player_id, start_time, end_time) \
            \ VALUES (?, ?, ?, ?);"
    executeMany sql (insertManyAvailability req)

insertManyAvailability :: Availability -> [(GameID, PlayerID, UTCTime, UTCTime)]
insertManyAvailability Availability{..} = foldl' (\acc (Interval s e) -> (_addGameID, _addPlayerID, s, e) : acc) [] _addAvailability

selectAvailability :: (WithDb env m, WithError m) => GameID -> PlayerID -> m Availability
selectAvailability gID pID = do
    let sql =
            "SELECT game_id, player_id, start_time, end_time FROM availability \
            \ WHERE game_id = ? AND player_id = ?"
    buildAvailability =<< queryWith parseAvailability sql (gID, pID)

parseAvailability :: RowParser (GameID, PlayerID, Interval)
parseAvailability = do
    gID <- field
    pID <- field

    start <- field
    end <- field

    return (gID, pID, Interval start end)

buildAvailability :: (WithError m) => [(GameID, PlayerID, Interval)] -> m Availability
buildAvailability [] = throwError singleRowError
buildAvailability rows@((gID, pID, _) : _) = return $ foldr (\(_, _, i) avail -> over addAvailability (i :) avail) (Availability gID pID []) rows
