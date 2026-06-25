-- | Endpoints for adding times to games and booking sessions through availability solving.
module Lib.Server.Availability where

import Lib.Core.Availability (Availability (..))
import Lib.Core.Game (GameID (..))
import Lib.Core.Player (PlayerID (..))
import Lib.Core.UserSession (UserSession)
import Lib.Db.Availability (insertAvailability, selectAvailability)
import Lib.Server.Common (AppServer, WithAuth, requireAuth)
import Servant (Capture, Get, JSON, NoContent (NoContent), Post, ReqBody, (:-), (:>))
import Servant.Auth.Server (AuthResult)

data AvailabilityRoutes route = AvailabilityRoutes
    { addAvailability ::
        route :- ReqBody '[JSON] Availability :> Post '[JSON] NoContent
    , getAvailability :: route :- Capture "gameID" Int :> Capture "playerID" Int :> Get '[JSON] Availability
    }
    deriving (Generic)

-- TODO require player
postAvailabilityHandler :: (WithAuth env m) => Availability -> m NoContent
postAvailabilityHandler req = insertAvailability req $> NoContent

getAvailabilityHandler :: (WithAuth env m) => Int -> Int -> m Availability
getAvailabilityHandler gID pID = selectAvailability (GameID gID) (PlayerID pID)

availabilityServer :: AuthResult UserSession -> AvailabilityRoutes AppServer
availabilityServer token =
    AvailabilityRoutes
        { addAvailability = \availabilityRequest -> requireAuth token $ \_ -> postAvailabilityHandler availabilityRequest
        , getAvailability = \gID pID -> requireAuth token $ \_ -> getAvailabilityHandler gID pID
        }
