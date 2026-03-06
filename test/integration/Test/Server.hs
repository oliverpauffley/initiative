module Test.Server where

import qualified Data.UUID as UUID
import Lib.App (App, AppEnv)
import Lib.App.Env ()
import Lib.App.Error (missingHeader, notAllowed)
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (NewGameRequest))
import Lib.Core.Player (Email (..), NewPlayerRequest (..))
import Lib.Core.UserSession (SessionToken (..))
import Lib.Db.Functions (singleRowError)
import Lib.Db.Player (insertPlayer)
import Lib.Db.Schema (prepareDB)
import Lib.Db.UserSession (createSession)
import Lib.Effects.Log (runAppLogIO_)
import Lib.Server.Auth (addNewUserHandler)
import Lib.Server.Game
import Test.Assert (equals, failsWith)
import Test.Common (joinSpecs)
import Test.Hspec (before_)
import Test.Hspec.Core.Spec

serverSpecs :: AppEnv -> Spec
serverSpecs =
    joinSpecs
        "Api"
        [ gamesSpec
        , authSpec
        ]

-- | Create a non-admin player session for use in tests.
withTestSession :: (Text -> App a) -> App a
withTestSession action = do
    pid <- insertPlayer (NewPlayerRequest "testuser" (Email "test@example.com"))
    token <- createSession pid
    action (UUID.toText (unSessionToken token))

gamesSpec :: AppEnv -> Spec
gamesSpec env = before_ (runAppLogIO_ env prepareDB) $ describe "Games" $ do
    it "should reject unauthenticated requests" $
        env & getAllGamesHandler Nothing `failsWith` missingHeader "X-Session-Token"
    it "should return a 404 on an unknown game" $
        env & withTestSession (\tok -> getGameHandler (Just tok) 100) `failsWith` singleRowError
    it "should accept a new game" $
        equals
            ( withTestSession $ \tok ->
                postNewGameHandler (Just tok) (NewGameRequest "test-game" "test-system")
            )
            (Game (GameID 1) "test-game" "test-system" [])
            env
    it "should return all games" $
        equals
            ( withTestSession $ \tok -> do
                _ <- postNewGameHandler (Just tok) (NewGameRequest "test-game" "test-system")
                _ <- postNewGameHandler (Just tok) (NewGameRequest "test-game-2" "test-system-2")
                getAllGamesHandler (Just tok)
            )
            [ Game (GameID 1) "test-game" "test-system" []
            , Game (GameID 2) "test-game-2" "test-system-2" []
            ]
            env

authSpec :: AppEnv -> Spec
authSpec env = before_ (runAppLogIO_ env prepareDB) $ describe "Auth" $ do
    it "should reject addNewUser without a session" $
        env
            & addNewUserHandler Nothing (NewPlayerRequest "player name" (Email "player@example.com"))
            `failsWith` missingHeader "X-Session-Token"
    it "should reject addNewUser for non-admin users" $
        env
            & withTestSession
                (\tok -> addNewUserHandler (Just tok) (NewPlayerRequest "player name" (Email "player@example.com")))
            `failsWith` notAllowed "Admin access required"
