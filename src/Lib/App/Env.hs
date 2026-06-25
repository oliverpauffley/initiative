module Lib.App.Env (
    Env (..),
    grab,
    Has (..),
    DbPool,
    GoogleClientID,
) where

import Colog (HasLog (..), LogAction, Message)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Servant.Auth.Server (CookieSettings, JWTSettings)
import URI.ByteString (Port)

-- Type alias for postgresconnection
type DbPool = Pool Connection

type GoogleClientID = Text

data Env (m :: Type -> Type) = Env
    { envDbPool :: !DbPool
    , envPort :: !Port
    , envLogAction :: !(LogAction m Message)
    , envHttpManager :: !Manager
    , envGoogleClientID :: !GoogleClientID
    , envCookieSettings :: !CookieSettings
    , envJWTSettings :: !JWTSettings
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env{envLogAction = newAction}
    {-# INLINE setLogAction #-}

class Has field env where
    obtain :: env -> field

instance Has DbPool (Env m) where obtain = envDbPool
instance Has Port (Env m) where obtain = envPort
instance Has (LogAction m Message) (Env m) where obtain = envLogAction
instance Has Manager (Env m) where obtain = envHttpManager
instance Has GoogleClientID (Env m) where obtain = envGoogleClientID
instance Has CookieSettings (Env m) where obtain = envCookieSettings
instance Has JWTSettings (Env m) where obtain = envJWTSettings

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
