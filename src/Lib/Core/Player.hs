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
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromJSON, ToJSON)

-- | Identify the auth provider of oauth. Apparently there is a risk to not tracking the auth provider since we want to track the email provider with it's auth pair.
data AuthProviders = Google
    deriving stock (Generic, Show, Eq)

-- deriving newtype (ToField, FromField, FromJSON, ToJSON)

data Player = Player
    { playerID :: !PlayerID
    , playerName :: !Text
    , playerEmail :: !Email
    -- , playerAuthProvider :: AuthProviders
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToRow, FromRow)
    deriving (FromJSON, ToJSON)
