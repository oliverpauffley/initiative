{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Helper functions for setting up and tearing down the databases
module Lib.Db.Schema where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (..))
import Lib.Db (executeRaw)
import Lib.Db.Functions (WithDb)

setupDB :: (WithDb env m) => m ()
setupDB =
    executeRaw
        [sql|
           create TABLE IF NOT EXISTS games (
           id SERIAL PRIMARY KEY
         , name TEXT NOT NULL
         , system TEXT NOT NULL
         , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
        );


        create TABLE IF NOT EXISTS sessions (
           id SERIAL PRIMARY KEY
         , game_id INT REFERENCES games(id)
         , start_time TIMESTAMP WITH TIME ZONE NOT NULL
         , end_time TIMESTAMP WITH TIME ZONE NOT NULL
         , name TEXT NOT NULL
        );


        create TABLE IF NOT EXISTS players (
          id       SERIAL  PRIMARY KEY
        , name     TEXT    NOT NULL
        , email    TEXT    NOT NULL
        , is_admin BOOLEAN NOT NULL DEFAULT FALSE
        );


        create TABLE IF NOT EXISTS user_sessions (
          token      UUID PRIMARY KEY
        , player_id  INT  NOT NULL REFERENCES players(id) ON DELETE CASCADE
        , expires_at TIMESTAMP WITH TIME ZONE NOT NULL
        )
|]

executeFile :: (WithDb env m) => FilePath -> m ()
executeFile path = do
    sqlStatements <- readFileBS path
    executeRaw $ Query sqlStatements

-- | prepareDB gets the database setup for testing
prepareDB :: (WithDb env m) => m ()
prepareDB = teardownDb >> setupDB

teardownDb :: (WithDb env m) => m ()
teardownDb =
    executeRaw
        [sql|
        DROP TABLE IF EXISTS user_sessions;
        DROP TABLE IF EXISTS sessions;
        DROP TABLE IF EXISTS games;
        DROP TABLE IF EXISTS players;
        |]
