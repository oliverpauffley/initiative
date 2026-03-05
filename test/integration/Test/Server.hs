module Test.Server where

import Lib.App (AppEnv)
import Lib.App.Env ()
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (NewGameRequest))
import Lib.Core.Player (Email (..), NewPlayerRequest (..), PlayerID (PlayerID))
import Lib.Db.Functions (WithDb, singleRowError)
import Lib.Server
import Lib.Server.Auth (AuthRoutes (addNewUser), addNewUserHandler, loginHandler)
import Lib.Server.Game
import Test.Assert (equals, failsWith, succeeds)
import Test.Common (joinSpecs)
import Test.Hspec.Core.Spec

serverSpecs :: AppEnv -> Spec
serverSpecs =
    joinSpecs
        "Api"
        [ gamesSpec
        , authSpec
        ]

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

authSpec :: AppEnv -> Spec
authSpec env = describe "Auth" $ do
    it "should return a 404 error when user is unknown" $
        env & loginHandler "unknownemail" `failsWith` singleRowError
    it "should return the player ID for a known player" $
        env & equals (addNewUserHandler (NewPlayerRequest "player name" (Email "player email")) >> loginHandler "player email") (PlayerID 1)
