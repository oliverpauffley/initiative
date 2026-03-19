-- | Endpoints for adding times to games and booking sessions through availability solving.
module Lib.Server.Availability where

import Lib.Core.Availability (Availability (..))
import Lib.Core.Game (GameID (..))
import Lib.Core.Player (PlayerID (..))
import Lib.Db.Availability (insertAvailability, selectAvailability)
import Lib.Server.Common (AppServer, WithAuth, requirePlayer)
import Servant (Capture, Get, Header, JSON, NoContent (NoContent), Post, ReqBody, (:-), (:>))

data AvailabilityRoutes route = AvailabilityRoutes
    { addAvailability ::
        route :- Header "X-Session-Token" Text :> ReqBody '[JSON] Availability :> Post '[JSON] NoContent
    , getAvailability :: route :- Header "X-Session-Token" Text :> Capture "gameID" Int :> Capture "playerID" Int :> Get '[JSON] Availability
    }
    deriving (Generic)

postAvailabilityHandler :: (WithAuth env m) => Maybe Text -> Availability -> m NoContent
postAvailabilityHandler mHeader req@Availability{..} = (requirePlayer mHeader _addPlayerID *> insertAvailability req) $> NoContent

getAvailabilityHandler :: (WithAuth env m) => Maybe Text -> Int -> Int -> m Availability
getAvailabilityHandler mHeader gID pID = requirePlayer mHeader (PlayerID pID) *> selectAvailability (GameID gID) (PlayerID pID)

availabilityServer :: AvailabilityRoutes AppServer
availabilityServer =
    AvailabilityRoutes
        { addAvailability = postAvailabilityHandler
        , getAvailability = getAvailabilityHandler
        }
