module Score where

import Lib

-- A trick is a completed sequence of plays
-- It always has a winner
data Trick = Trick [Play] Trump deriving (Show, Eq, Ord)

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
