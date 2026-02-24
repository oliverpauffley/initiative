-- | MonadReader wrappers around @postgres-simple@ library.
module Lib.Db.Functions where

import Lib.App.Env (DbPool, Has)

import Lib.App.Error (AppErrorType, WithError, dbError, throwError, throwOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser)
import Lib.App (grab)

-- | Constraint for monadic actions that wants access to the database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

initialisePool :: ByteString -> IO DbPool
initialisePool creds = Pool.newPool $ Pool.defaultPoolConfig (Sql.connectPostgreSQL creds) Sql.close 10 5

-- | Perform action that needs a connection to the database
withPool :: (WithDb env m) => (Sql.Connection -> IO b) -> m b
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f

{- | Performs a query with arguments and returns the resulting rows with the
given parameters.
-}
query ::
    (WithDb env m, Sql.ToRow args, Sql.FromRow res) =>
    Sql.Query ->
    args ->
    m [res]
query q args = withPool $ \conn -> Sql.query conn q args
{-# INLINE query #-}

-- | Runs a query with a row parser.
queryWith :: (WithDb env m, Sql.ToRow args, WithError m) => RowParser res -> Sql.Query -> args -> m [res]
queryWith r q args = withPool $ \conn -> Sql.queryWith r conn q args
{-# INLINE queryWith #-}

-- | Runs a query with a row parser with no args.
queryWith_ :: (WithDb env m, WithError m) => RowParser res -> Sql.Query -> m [res]
queryWith_ r q = withPool $ \conn -> Sql.queryWith_ r conn q
{-# INLINE queryWith_ #-}

-- | Executes a query without arguments that is not expected to return results.
executeRaw ::
    (WithDb env m) =>
    Sql.Query ->
    m ()
executeRaw q = withPool $ \conn -> void $ Sql.execute_ conn q
{-# INLINE executeRaw #-}

-- | Performs a query without arguments and returns the resulting rows.
queryRaw ::
    forall res env m.
    (WithDb env m, FromRow res) =>
    Sql.Query ->
    m [res]
queryRaw q = withPool $ \conn -> Sql.query_ conn q
{-# INLINE queryRaw #-}

-- | Executes a query with parameters that is not expected to return results.
execute ::
    forall args env m.
    (WithDb env m, Sql.ToRow args) =>
    Sql.Query ->
    args ->
    m ()
execute q args = withPool $ \conn -> void $ Sql.execute conn q args
{-# INLINE execute #-}

-- | Executes a multi-row query that is not expected to return results.
executeMany ::
    (WithDb env m, Sql.ToRow args) =>
    Sql.Query ->
    [args] ->
    m ()
executeMany q args = withPool $ \conn -> void $ Sql.executeMany conn q args
{-# INLINE executeMany #-}

returning ::
    (WithDb env m, Sql.ToRow args, FromRow res) =>
    Sql.Query ->
    [args] ->
    m [res]
returning q args = withPool $ \conn -> Sql.returning conn q args
{-# INLINE returning #-}

-- Error Helpers

{- | Helper function working with results from a database when you expect
only one row to be returned.
-}
asSingleRow :: (WithError m) => m [a] -> m a
asSingleRow res =
    withFrozenCallStack $
        throwOnNothingM
            singleRowError
            (viaNonEmpty head <$> res)

singleRowError :: AppErrorType
singleRowError = dbError "Expected a single row, but got none"
