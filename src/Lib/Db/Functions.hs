-- | MonadReader wrappers around @postgres-simple@ library.
module Lib.Db.Functions where

import Lib.App.Env (DbPool, Has)

-- import Lib.App.Error (AppErrorType, WithError, dbError, throwError, throwOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql

-- | Constraint for monadic actions that wants access to the database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

initialisePool :: ByteString -> IO DbPool
initialisePool creds = Pool.newPool $ Pool.defaultPoolConfig (Sql.connectPostgreSQL creds) Sql.close 10 5
