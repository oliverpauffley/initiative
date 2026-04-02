{-# LANGUAGE DeriveAnyClass #-}

-- | Games are the DM created groupings for a TTRPG.
module Lib.Core.Game (Game (..), Session (..), GameID (..), NewGameRequest (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Lib.Core.Interval (Interval)
import Lib.Core.Player (PlayerID)

newtype GameID = GameID {gID :: Int}
    deriving stock (Generic, Show, Eq, Ord)
    deriving anyclass (FromRow)
    deriving newtype (ToField, FromField, FromJSON, ToJSON)

-- | A game with a name and system. We might have multiple games for the same system but that should be different if it makes sense to the DM.
data Game = Game
    { gameID :: !GameID
    , -- This is the Player ID for the player running the game.
      gamePlayerID :: !PlayerID
    , gameName :: !Text
    , gameSystem :: !Text
    , gameSessions :: ![Session]
    }
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON, ToJSON)

data NewGameRequest = NewGameRequest
    { newGameName :: !Text
    , newGameSystem :: !Text
    , newGamePlayerID :: !PlayerID
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToRow)
    deriving (FromJSON)

-- | A session is an interval in time from start to finish. The name is optional and will be created from the attached game if unset.
data Session = Session
    { sessionInterval :: !Interval
    , sessionName :: !(Maybe Text)
    -- , -- The players for the game
    --  players :: ![PlayerID]
    }
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON, ToJSON)
