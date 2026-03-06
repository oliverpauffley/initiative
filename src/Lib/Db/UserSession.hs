module Lib.Db.UserSession (createSession, lookupSession, deleteSession) where

import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Only (..))
import Lib.App (WithError)
import Lib.Core.Player (PlayerID)
import Lib.Core.UserSession (SessionToken (..), UserSession)
import Lib.Db.Functions (WithDb, asSingleRow, execute, query)

sessionTTL :: NominalDiffTime
sessionTTL = 60 * 60 * 24 * 7 -- 7 days

createSession :: (WithDb env m, WithError m, MonadIO m) => PlayerID -> m SessionToken
createSession pID = do
    token <- liftIO $ SessionToken <$> nextRandom
    now <- liftIO getCurrentTime
    let expiresAt = addUTCTime sessionTTL now
    let sql = "INSERT INTO user_sessions (token, player_id, expires_at) VALUES (?, ?, ?);"
    execute sql (token, pID, expiresAt)
    pure token

lookupSession :: (WithDb env m, WithError m) => SessionToken -> m UserSession
lookupSession token = do
    let sql =
            "SELECT token, player_id, expires_at \
            \FROM user_sessions \
            \WHERE token = ? AND expires_at > NOW();"
    asSingleRow $ query sql (Only token)

deleteSession :: (WithDb env m, WithError m) => SessionToken -> m ()
deleteSession token = do
    let sql = "DELETE FROM user_sessions WHERE token = ?;"
    execute sql (Only token)
