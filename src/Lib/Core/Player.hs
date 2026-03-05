{-# LANGUAGE DeriveAnyClass #-}

-- | Players or DMs are people who attend or run games. The difference between DMs and players is just if they create the game or guests who sign up to play.
module Lib.Core.Player where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype PlayerID = PlayerID {pID :: Int}
    deriving stock (Generic, Show, Eq, Ord)
    deriving anyclass (FromRow)
    deriving newtype (ToField, FromField, FromJSON, ToJSON)

newtype Email = Email
    { unEmail :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToRow, FromRow)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromJSON, ToJSON)

data Player = Player
    { playerID :: !PlayerID
    , playerName :: !Text
    , playerEmail :: !Email
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToRow, FromRow)
    deriving (FromJSON, ToJSON)

data NewPlayerRequest = NewPlayerRequest
    { newPlayerName :: !Text
    , newPlayerEmail :: !Email
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToRow)
    deriving (FromJSON)
