module Test.Server where

import Lib.App (AppEnv)
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (NewGameRequest))
import Lib.Db.Functions (WithDb, singleRowError)
import Lib.Server (Site (getGame), getGameHandler, postNewGameHandler)
import Test.Assert (equals, failsWith)
import Test.Common (joinSpecs)
import Test.Hspec.Core.Spec

serverSpecs :: AppEnv -> Spec
serverSpecs =
    joinSpecs
        "Api"
        [gamesSpec]

gamesSpec :: AppEnv -> Spec
gamesSpec env = describe "Games" $ do
    it "should return a 404 on an unknown game" $
        env & getGameHandler 100 `failsWith` singleRowError
    it "should accept a new game" $
        equals
            ( postNewGameHandler
                (NewGameRequest "test-game" "test-system")
            )
            (Game (GameID 1) "test-game" "test-system" [])
            env
