module Lib.Db.Player where

import Database.PostgreSQL.Simple (Only (..))
import Lib.App (WithError)
import Lib.Core.Player (Email, NewPlayerRequest, Player, PlayerID)
import Lib.Db (WithDb, asSingleRow, query, queryRaw)

getPlayer :: (WithDb env m, WithError m) => Email -> m Player
getPlayer email = do
    let sql = "SELECT id, name, email, is_admin FROM players WHERE email = ?;"
    asSingleRow $ query sql email

getPlayerByID :: (WithDb env m, WithError m) => PlayerID -> m Player
getPlayerByID pID = do
    let sql = "SELECT id, name, email, is_admin FROM players WHERE id = ?;"
    asSingleRow $ query sql (Only pID)

getPlayers :: (WithDb env m, WithError m) => m [Player]
getPlayers = do
    let sql = "SELECT id, name, email, is_admin FROM players;"
    queryRaw sql

insertPlayer :: (WithDb env m, WithError m) => NewPlayerRequest -> m PlayerID
insertPlayer req = do
    let sql = "INSERT INTO players (name, email) VALUES (?, ?) RETURNING id;"
    asSingleRow $ query sql req
