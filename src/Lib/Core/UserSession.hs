{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.UserSession (SessionToken (..), UserSession (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Lib.Core.Player (PlayerID)

newtype SessionToken = SessionToken {unSessionToken :: UUID}
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromField, ToField, FromJSON, ToJSON)

data UserSession = UserSession
    { sessionToken :: !SessionToken
    , sessionPlayerID :: !PlayerID
    , sessionExpiresAt :: !UTCTime
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromRow, ToRow)
