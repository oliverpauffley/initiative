module Lib.Core.BookingSpec (spec) where

import Lib.Core.Booking
import Test.Hspec

import qualified Data.Map.Strict as Map (elems, empty, insertWith)
import qualified Data.Set.NonEmpty as NESet
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec =
    describe "book" $ do
        it "returns all when all match" $
            let a = neSetFromList [1, 2, 3]
             in book' 3 3 10 (PA (("scott", a) :| [("bob", a), ("john", a)]))
                    `shouldBe` Just
                        ( ("bob" :| ["john", "scott"], 1)
                            :| [ ("bob" :| ["john", "scott"], 2)
                               , ("bob" :| ["john", "scott"], 3)
                               ]
                        )
        it "returns one when it's the only match" $
            let a = neSetFromList [1]
             in book' 1 3 10 (PA (("scott", a) :| [("bob", a), ("john", a)]))
                    `shouldBe` Just (("bob" :| ["john", "scott"], 1) :| [])
        specSpread
        it "always returns availability from the first players availability" $ hedgehog $ do prop_contained
        it "returns all dm sessions when minimum players is zero" $ hedgehog $ do prop_zero_target_players
        it "should always return Uptos longer or the same length as AtLeasts" $ hedgehog $ do prop_least_less_than_upto

-- | Generate an availability with the given range for generate the dates
genAvailabilityWithRange :: (MonadGen m) => Range.Range Int -> m (String, NESet.NESet Int)
genAvailabilityWithRange r = do
    name <- Gen.string (Range.linear 1 10) Gen.alpha
    dates <- NESet.fromList <$> Gen.nonEmpty r (Gen.int (Range.linear 1 100))
    return (name, dates)

genAvailability :: (MonadGen m) => m (String, NESet.NESet Int)
genAvailability = genAvailabilityWithRange (Range.linear 1 20)

genAvailabilitiesWithRange :: (MonadGen m) => Range.Range Int -> m (NonEmpty (String, NESet.NESet Int))
genAvailabilitiesWithRange r = Gen.nonEmpty r genAvailability

genAvailabilities :: (MonadGen m) => m (NonEmpty (String, NESet.NESet Int))
genAvailabilities = genAvailabilitiesWithRange (Range.linear 1 8)

-- all picked availabilities must be contained within the DM availabilites (which is the first in the non empty).
prop_contained :: PropertyT IO ()
prop_contained =
    do
        avails@(dm :| rest) <- forAll genAvailabilities
        numberSessions <- forAll $ Gen.int (Range.linear 1 (length dm))
        minPlayers <- forAll $ Gen.int (Range.linear 1 (length rest))
        case book' numberSessions minPlayers 10 (PA avails) of
            Nothing -> success
            (Just sessions) ->
                let slots = fmap snd sessions
                    dmSlots = fmap snd sessions
                 in assert $ all (`elem` dmSlots) slots

-- with zero players as the target we will always book every DM session
prop_zero_target_players :: PropertyT IO ()
prop_zero_target_players =
    do
        avails@(dm :| _) <- forAll genAvailabilities
        numberSessions <- forAll $ Gen.int (Range.linear 1 (length dm))
        case book' numberSessions 0 10 (PA avails) of
            Nothing -> success
            (Just sessions) ->
                let slots = fmap snd sessions
                    dmSlots = fmap snd sessions
                 in dmSlots === slots

-- we know that booking with `AtLeast` should always be less than or equal to `UpTo`
prop_least_less_than_upto :: PropertyT IO ()
prop_least_less_than_upto =
    do
        avails@(dm :| _) <- forAll genAvailabilities
        numberSessions <- forAll $ Gen.int (Range.linear 1 (length dm))
        let gotLeast = book (AtLeast numberSessions) (PA avails)
            gotUpto = book (UpTo numberSessions) (PA avails)
        case (gotLeast, gotUpto) of
            (Nothing, _) -> success
            (_, Nothing) -> success
            (Just sLeast, Just sUpto) -> assert $ length sLeast >= length sUpto

specSpread :: Spec
specSpread =
    it "should spread the sessions around equally when over max players" $ do
        let a =
                neSetFromList
                    [1, 2, 3]
            avails = ("scott", a) :| [("bob", a), ("john", a), ("ryan", a), ("owen", a)]
         in -- the number of sessions is always some multiple of the players and max number of players

            case book' 2 2 2 (PA avails) of
                Nothing -> expectationFailure "should have returned something"
                (Just sessions) ->
                    let playerGameCount = Map.elems $ foldr (\(ps, _) m -> foldr (\p m' -> if p == "scott" then m else Map.insertWith (+) p 1 m') m ps) Map.empty sessions
                     in playerGameCount `shouldSatisfy` allEqual

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

neSetFromList :: (Ord a) => [a] -> NESet.NESet a
neSetFromList xs = case NESet.nonEmptySet $ fromList xs of
    Nothing -> error "empty list"
    Just s -> s
