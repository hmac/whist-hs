module Score where

import Lib

-- A trick is a completed sequence of plays
-- It always has a winner
data Trick = Trick [Play] Trump deriving (Show, Eq, Ord)

data Round = Round [Trick]

-- Returns the play that wins the trick
-- This should be either:
--   the highest trump, if there are any
--   the highest card
winner :: Trick -> Play
winner (Trick plays trump) = case filterSuit trump plays of
                               [] -> maximum plays
                               trumps -> maximum trumps

