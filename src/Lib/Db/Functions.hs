-- | MonadReader wrappers around @postgres-simple@ library.
module Lib.Db.Functions where

import Lib.App.Env (DbPool, Has)

-- import Lib.App.Error (AppErrorType, WithError, dbError, throwError, throwOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.FromRow (RowParser)
import Lib.App (WithError, grab)

-- | Constraint for monadic actions that wants access to the database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

initialisePool :: ByteString -> IO DbPool
initialisePool creds = Pool.newPool $ Pool.defaultPoolConfig (Sql.connectPostgreSQL creds) Sql.close 10 5

-- | Perform action that needs a connection to the database
withPool :: (WithDb env m) => (Sql.Connection -> IO b) -> m b
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f

queryWith_ :: (WithDb env m, WithError m) => RowParser res -> Sql.Query -> m [res]
queryWith_ r q = withPool $ \conn -> Sql.queryWith_ r conn q
