module Score where

import Lib

-- A trick is a completed sequence of plays
-- It always has a winner
data Trick = Trick
  { plays :: [Play]
  , trump :: Trump
  } deriving (Show, Eq, Ord)

-- A round is a series of tricks
data Round = Round [Trick]

-- A game is a series of rounds
data Game = Game [Round]

-- Returns the play that wins the trick
-- This should be either:
--   the highest trump, if there are any
--   the highest card
winningPlay :: Trick -> Play
winningPlay (Trick plays trump) = case filterSuit trump plays of
                               [] -> maximum plays
                               trumps -> maximum trumps

instance Winnable Trick where
  winner trick = case winningPlay trick of
               (Play _ player) -> player


instance Winnable Round where
  -- The winner of a round is the player with the most tricks
  -- Note: This is NOT the same as the player with the most points!

class Winnable a where
  winner :: a -> Player

makePlay :: Trick -> Hand -> Play -> Trick
-- ensure hand belongs to same player as play
-- ensure player has not already played in this trick
-- ensure play is valid
makePlay trick (Hand p1 _) (Play _ p2) | p1 /= p2 = trick
makePlay trick@(Trick plays trump) hand play@(Play _ player)
  | elem player (map (\(Play _ p) -> p) plays) = trick
  | not (validPlay play hand trump plays) = trick
makePlay (Trick plays trump) _ play = Trick (play:plays) trump
