{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Taking a list of availability for players find possible sessions to be booked.
module Lib.Core.Booking (
    PlayerAvailability (..),
    Sessions,
    BookingStrategy (..),
    MinPlayers,
    MaxPlayers,
    TargetSessions,
    book,
    book',
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set.NonEmpty as NESet

{- | Possible player availabilities for games. It is expected that the DMs availability is first
but this only matters for the `WestMarches` games where the players are possibly different each session.
-}
newtype PlayerAvailability k a = PA {unPA :: NonEmpty (k, NESet.NESet a)}
    deriving stock (Eq, Show, Foldable)

type Sessions k a = NonEmpty (NonEmpty k, a)

-- | The smallest number of players to book for a session. If the availability is lower than this it will not be booked.
type MinPlayers = Int

-- | When booking multiple sessions at a time this is the number to book.
type TargetSessions = Int

-- | The highest number of players to book for a session. When the number of available players is higher than this the player with the lowest frequency in other sessions will be picked
type MaxPlayers = Int

data BookingStrategy
    = -- | Return at least the given target number of sessions.
      AtLeast TargetSessions
    | -- | Up to the number of target sessions.
      UpTo TargetSessions
    | -- | Book target sessions with any players above the minimum.
      WestMarches TargetSessions MinPlayers MaxPlayers
    deriving stock (Show, Eq, Generic)

book :: (Ord k, Ord a) => BookingStrategy -> PlayerAvailability k a -> Maybe (Sessions k a)
book strategy availability = case strategy of
    AtLeast target -> do
        bookings <- book (UpTo target) availability
        guard (length bookings >= target)
        pure $ NE.fromList (NE.take target bookings)
    UpTo target ->
        book (WestMarches target (playerCount - 1) (playerCount - 1)) availability
      where
        playerCount = length availability
    WestMarches sessions minP maxP ->
        book' sessions minP maxP availability

book' :: (Ord a, Ord k) => TargetSessions -> MinPlayers -> MaxPlayers -> PlayerAvailability k a -> Maybe (Sessions k a)
book' targetSessions minPlayers maxPlayers as = fmap NE.reverse $ nonEmpty $ go (toList dmAvailability) availabilities []
  where
    (PA ((dmID, dmAvailability) :| availabilities)) = as

    -- we got the number of sessions we want so return them.
    go _ _ m | length m == targetSessions = m
    -- we ran out of sessions so return what we have.
    go [] _ m = m
    -- for each dm session check which players are available and add them.
    go (interval : ds) xs m = case foldr (match interval) (NE.singleton dmID) xs of
        matches
            | length matches > maxPlayers -> go ds xs ((cutPlayers m matches, interval) : m)
            | length matches >= minPlayers -> go ds xs ((matches, interval) : m)
        _otherwise -> go ds xs m

    -- if player has the given target in their availabilities then add them to the players.
    match target (pId, playerAvailability) acc
        | target `elem` playerAvailability = pId `NE.cons` acc
        | otherwise = acc

    cutPlayers :: (Ord k) => [(NonEmpty k, a)] -> NonEmpty k -> NonEmpty k
    cutPlayers sessions matches = fromMaybe matches . nonEmpty . fmap snd . take maxPlayers $ playerRanks
      where
        playerGames = counts . flatten . map fst $ sessions
        playerRanks = sortBy (comparing fst) ((\p -> (Map.findWithDefault 0 p playerGames, p)) <$> toList matches)

flatten :: [NonEmpty a] -> [a]
flatten = concatMap toList

-- >>> counts ["a", "b", "a", "b", "c"]
-- fromList [("a",2),("b",2),("c",1)]
counts :: (Ord k) => [k] -> Map.Map k Int
counts = Map.fromListWith (+) . map (,1)
