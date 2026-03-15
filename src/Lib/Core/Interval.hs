{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Core.Interval where

import Control.Lens (makeLenses)

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime (..))

-- | Interval is a time period from given start to finish
data Interval = Interval
    { _iStart :: !UTCTime
    , _iEnd :: !UTCTime
    }
    deriving stock (Generic, Show, Eq, Ord)
    deriving (FromJSON, ToJSON)

$(makeLenses ''Interval)
