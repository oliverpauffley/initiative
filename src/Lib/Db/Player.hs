module Lib.Db.Player where

import Lib.App (WithError)
import Lib.Core.Player (Email, NewPlayerRequest, Player, PlayerID)
import Lib.Db (WithDb, asSingleRow, query, queryRaw)

getPlayer :: (WithDb env m, WithError m) => Email -> m Player
getPlayer email = do
    let sql = "SELECT id, name, email FROM players WHERE email = ?;"
    asSingleRow $ query sql email

getPlayers :: (WithDb env m, WithError m) => m [Player]
getPlayers = do
    let sql = "SELECT id, name, email FROM players;"
    queryRaw sql

insertPlayer :: (WithDb env m, WithError m) => NewPlayerRequest -> m PlayerID
insertPlayer req = do
    let sql = "INSERT INTO players (name, email) VALUES (?, ?) RETURNING id;"
    asSingleRow $ query sql req
