module Lib where

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Effects.Log (mainLogAction)

import Lib.Db (initialisePool)
import Lib.Server (application)
import Network.Wai.Handler.Warp (run)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO Configuration
    envDbPool <- initialisePool cDbCredentials

    -- pure config
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env = do
    run 8080 $ application env

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
