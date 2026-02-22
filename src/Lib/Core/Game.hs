{-# LANGUAGE DeriveAnyClass #-}

-- | Games are the DM created groupings for a TTRPG.
module Lib.Core.Game (Game (..), Session (..), GameID (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime (..))
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype GameID = GameID Int
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (ToField, FromField, FromJSON, ToJSON)

-- | A game with a name and system. We might have multiple games for the same system but that should be different if it makes sense to the DM.
data Game = Game
    { gameID :: !GameID
    , gameName :: !Text
    , gameSystem :: !Text
    , gameSessions :: ![Session]
    }
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON, ToJSON)

-- | A session is an interval in time from start to finish. The name is optional and will be created from the attached game if unset.
data Session = Session
    { sessionStart :: !UTCTime
    , sessionEnd :: !UTCTime
    , sessionName :: !(Maybe Text)
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromRow)
    deriving (FromJSON, ToJSON)
