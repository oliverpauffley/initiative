-- | Anything that needs to be shared between multiple sub components of the server package
module Lib.Server.Common where

import Lib.App (App)
import Servant.Server.Generic (AsServerT)

type AppServer = AsServerT App
