module Test.Assert where

import Lib.App (App, AppEnv, AppError (..), runAppAsIO)
import Lib.App.Error (AppErrorType)
import Test.Hspec (Expectation, expectationFailure, shouldBe, shouldSatisfy)

-- | Checks whether action fails and returns given error.
failsWith :: (Show a) => App a -> AppErrorType -> AppEnv -> Expectation
failsWith app err env =
    runAppAsIO env app >>= \case
        Left AppError{..} -> appErrorType `shouldBe` err
        Right a ->
            expectationFailure $
                "Expected 'Failure' with: " <> show err <> " but got: " <> show a

-- | Checks whether return result of the action satisfies given predicate.
satisfies :: (Show a) => App a -> (a -> Bool) -> AppEnv -> Expectation
satisfies app p env =
    runAppAsIO env app >>= \case
        Left e -> expectationFailure $ "Expected 'Success' but got: " <> show e
        Right a -> a `shouldSatisfy` p

-- | Checks that given action runs successfully.
succeeds :: (Show a) => App a -> AppEnv -> Expectation
succeeds = (`satisfies` const True)

-- | Checks whether action returns expected value.
equals :: (Show a, Eq a) => App a -> a -> AppEnv -> Expectation
equals app v env =
    runAppAsIO env app >>= \case
        Right a -> a `shouldBe` v
        Left e -> expectationFailure $ "Expected 'Success' but got: " <> show e
