{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Core.Availability where

import Control.Lens (makeLenses)
import Lib.Core.Game (GameID)
import Lib.Core.Player (PlayerID)

import Data.Aeson (FromJSON, ToJSON)
import Lib.Core.Interval (Interval)

data Availability = Availability
    { _addGameID :: !GameID
    , _addPlayerID :: !PlayerID
    , _addAvailability :: ![Interval]
    }
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON, ToJSON)

$(makeLenses ''Availability)
