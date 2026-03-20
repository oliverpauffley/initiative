module Test.Server where

import Data.Time
import qualified Data.UUID as UUID
import Lib.App (App, AppEnv)
import Lib.App.Env ()
import Lib.App.Error (missingHeader, notAllowed)
import Lib.Core.Availability (Availability (Availability))
import Lib.Core.Game (Game (..), GameID (..), NewGameRequest (NewGameRequest))
import Lib.Core.Interval (Interval (..))
import Lib.Core.Player (Email (..), NewPlayerRequest (..), PlayerID (..))
import Lib.Core.UserSession (SessionToken (..))
import Lib.Db.Functions (singleRowError)
import Lib.Db.Player (insertPlayer)
import Lib.Db.Schema (prepareDB)
import Lib.Db.UserSession (createSession)
import Lib.Effects.Log (runAppLogIO_)
import Lib.Server.Auth (addNewUserHandler)
import Lib.Server.Availability (getAvailabilityHandler, postAvailabilityHandler)
import Lib.Server.Game
import Test.Assert (equals, equalsWith, failsWith)
import Test.Common (joinSpecs)
import Test.Hspec (before_)
import Test.Hspec.Core.Spec

-- | Truncate UTCTime to microsecond precision, matching Postgres TIME type.
truncateTime :: UTCTime -> UTCTime
truncateTime t = t{utctDayTime = fromIntegral (truncate (utctDayTime t * 1e6) :: Integer) / 1e6}

serverSpecs :: AppEnv -> Spec
serverSpecs =
    joinSpecs
        "Api"
        [ gamesSpec
        , authSpec
        , availabilitySpec
        ]

-- | Create a non-admin player session for use in tests.
withTestSession :: (Text -> App a) -> App a
withTestSession action = do
    pid <- insertPlayer (NewPlayerRequest "testuser" (Email "test@example.com"))
    token <- createSession pid
    action (UUID.toText (unSessionToken token))

-- | Create a non-admin player session with PlayerID
withTestPlayerSession :: (Text -> Int -> App a) -> App a
withTestPlayerSession action = do
    pid <- insertPlayer (NewPlayerRequest "testuser" (Email "test@example.com"))
    token <- createSession pid
    action (UUID.toText (unSessionToken token)) (pID pid)

gamesSpec :: AppEnv -> Spec
gamesSpec env = before_ (runAppLogIO_ env prepareDB) $ describe "Games" $ do
    it "should reject unauthenticated requests" $
        env & getAllGamesHandler Nothing `failsWith` missingHeader "X-Session-Token"
    it "should return a 404 on an unknown game" $
        env & withTestSession (\tok -> getGameHandler (Just tok) 100) `failsWith` singleRowError
    it "should accept a new game" $
        equals
            ( withTestPlayerSession $ \tok pid ->
                postNewGameHandler (Just tok) (NewGameRequest "test-game" "test-system" (PlayerID pid))
            )
            (Game (GameID 1) (PlayerID 1) "test-game" "test-system" [])
            env
    it "should return all games" $
        equals
            ( withTestPlayerSession $ \tok pid -> do
                _ <- postNewGameHandler (Just tok) (NewGameRequest "test-game" "test-system" (PlayerID pid))
                _ <- postNewGameHandler (Just tok) (NewGameRequest "test-game-2" "test-system-2" (PlayerID pid))
                getAllGamesHandler (Just tok)
            )
            [ Game (GameID 1) (PlayerID 1) "test-game" "test-system" []
            , Game (GameID 2) (PlayerID 1) "test-game-2" "test-system-2" []
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

availabilitySpec :: AppEnv -> Spec
availabilitySpec env = before_ (runAppLogIO_ env prepareDB) $ describe "Availability" $ do
    it "should reject unauthenticated requests" $
        env & getAvailabilityHandler Nothing 1 1 `failsWith` missingHeader "X-Session-Token"
    it "should return 404 on unknown availability for game" $
        env & withTestSession (\tok -> getAvailabilityHandler (Just tok) 1 1) `failsWith` singleRowError
    it "should accept a new availability" $ do
        start1 <- truncateTime <$> getCurrentTime
        end1 <- truncateTime <$> getCurrentTime
        start2 <- truncateTime <$> getCurrentTime
        end2 <- truncateTime <$> getCurrentTime
        env
            & equalsWith
                ( withTestPlayerSession
                    ( \tok pID ->
                        do
                            Game{..} <- postNewGameHandler (Just tok) (NewGameRequest "test-game" "test-system" (PlayerID pID))
                            postAvailabilityHandler (Just tok) (Availability gameID (PlayerID pID) [Interval start1 end1, Interval start2 end2])
                            *> getAvailabilityHandler (Just tok) 1 pID
                    )
                )
                (Availability (GameID 1) (PlayerID 1) [Interval start1 end1, Interval start2 end2])
                approxEqAvailability

approxEqAvailability :: Availability -> Availability -> Bool
approxEqAvailability (Availability aGID aPID aAvail) (Availability bGID bPID bAvail) = aGID == bGID && aPID == bPID && all (uncurry approxEqInterval) (zip aAvail bAvail)

approxEqInterval :: Interval -> Interval -> Bool
approxEqInterval (Interval aStart aEnd) (Interval bStart bEnd) = approxEqTime aStart bStart && approxEqTime aEnd bEnd

approxEqTime :: UTCTime -> UTCTime -> Bool
approxEqTime a b = abs (diffUTCTime a b) < 0.001
