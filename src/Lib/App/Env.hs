module Lib.App.Env (
    Env (..),
    grab,
    Has (..),
    DbPool,
) where

import Colog (HasLog (..), LogAction, Message)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (OAuth2)
import URI.ByteString (Absolute, Port, URIRef)

-- Type alias for postgresconnection
type DbPool = Pool Connection

data Env (m :: Type -> Type) = Env
    { envDbPool :: !DbPool
    , envPort :: !Port
    , envLogAction :: !(LogAction m Message)
    , envHttpManager :: !Manager
    , envOAuth :: !OAuth2
    , envUserInfoUri :: !(URIRef Absolute)
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
instance Has OAuth2 (Env m) where obtain = envOAuth
instance Has (URIRef Absolute) (Env m) where obtain = envUserInfoUri

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
