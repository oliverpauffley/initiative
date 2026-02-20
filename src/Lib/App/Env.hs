module Lib.App.Env (
    Env (..),
    DbPool,
) where

import Colog (HasLog (..), LogAction, Message)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

-- Type alias for postgresconnection
type DbPool = Pool Connection

data Env (m :: Type -> Type) = Env
    { envDbPool :: !DbPool
    , envLogAction :: !(LogAction m Message)
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env{envLogAction = newAction}
    {-# INLINE setLogAction #-}
