module Main (main) where

import Control.Exception (bracket)
import qualified Data.Pool as Pool
import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import Lib (mkAppEnv)
import Lib.App (AppEnv)
import Lib.App.Env (Env (..))
import Lib.Config (loadConfig)
import Lib.Db.Schema (prepareDB)
import Lib.Effects.Log (runAppLogIO_)
import Test.Common
import Test.Hspec (Spec, hspec, sequential)
import Test.Server

main :: IO ()
main =
    bracket
        (loadConfig >>= mkAppEnv)
        (\Env{..} -> Pool.destroyAllResources envDbPool)
        runTests
  where
    runTests :: AppEnv -> IO ()
    runTests env = do
        hSetEncoding stdout utf8
        hSetEncoding stdout utf8

        -- prepare db tables
        runAppLogIO_ env prepareDB

        hspec $ hspecTests env

hspecTests :: AppEnv -> Spec
hspecTests =
    sequential
        . joinSpecs
            "Initiative Integration Tests"
            [ serverSpecs
            ]
