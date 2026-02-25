module Test.Server where

import Lib.App (AppEnv)
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (NewGameRequest))
import Lib.Db.Functions (WithDb, singleRowError)
import Lib.Server (Site (getGame), getAllGamesHandler, getGameHandler, postNewGameHandler)
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
    it "should return all games" $
        equals
            -- add a new game then get both back
            ( postNewGameHandler (NewGameRequest "test-game-2" "test-system-2")
                >> getAllGamesHandler
            )
            [ Game (GameID 1) "test-game" "test-system" []
            , Game (GameID 2) "test-game-2" "test-system-2" []
            ]
            env
