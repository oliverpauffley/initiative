{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for setting up and tearing down the databases
module Lib.Db.Schema where

import Database.PostgreSQL.Simple.Types (Query (..))
import Lib.Db (executeRaw)
import Lib.Db.Functions (WithDb)

setupDB :: (WithDb env m) => m ()
setupDB = executeFile "sql/schema.sql"

executeFile :: (WithDb env m) => FilePath -> m ()
executeFile path = do
    sqlStatements <- readFileBS path
    executeRaw $ Query sqlStatements

-- | prepareDB gets the database setup for testing
prepareDB :: (WithDb env m) => m ()
prepareDB = teardownDb >> setupDB

teardownDb :: (WithDb env m) => m ()
teardownDb = executeFile "sql/drop.sql"
