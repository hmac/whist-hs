module Lib where

import System.Environment 
import Control.Monad
import Data.List

import Card (Card (Card), Suit, suit)


type Trump = Suit

-- TODO: embed the trump suit into Play?
data Play = Play Card Player deriving (Show, Eq)

data Round = Round [Trick] deriving (Show)

data Game = Game [Round] deriving (Show)


instance Ord Play where
  compare (Play c1 _) (Play c2 _) = compare c1 c2

data Player = Player String deriving (Show, Eq)

data Hand = Hand Player [Card] deriving (Show)


-- TODO: make nicer
cardFromPlay :: Play -> Card
cardFromPlay (Play card player) = card

-- TODO: remove this?
-- Returns the highest Play of a specific suit from a list of Plays
highest :: [Play] -> Suit -> Maybe Play
highest plays suit = case filterSuit suit plays of
  [] -> Nothing
  ps -> Just (maximum ps)

-- Filters a set of Plays into only those that have the specified suit
filterSuit :: Suit -> [Play] -> [Play]
filterSuit suit ps = filter (\ (Play (Card s _) _) -> s == suit) ps

-- Determines if a Play is valid. 
-- Checks that the card is in the player's hand
-- Checks that the player is following suit if they can
-- Checks that the player is trumping if they have trumps and can't follow suit
validPlay :: Play -> Hand -> Trump -> [Play] -> Bool
validPlay (Play card _) (Hand _ hand) trumps cardsPlayed  
  | not (any (elem card) [hand]) = False
  | and [ canFollowSuit, (not followingSuit) ] = False
  | and [ (not canFollowSuit), hasTrumps, (not trump) ] = False
  | otherwise = True
  where trump = (suit card) == trumps
        hasTrumps = any (\c -> (suit c) == trumps) hand
        suitLed = suit (cardFromPlay $ head cardsPlayed)
        isLeading = null cardsPlayed
        followingSuit = isLeading || (suit card) == suitLed
        canFollowSuit = isLeading || any (\c -> (suit c) == suitLed) hand

-- A trick is a completed sequence of plays
-- It always has a winner
data Trick = Trick
  { plays :: [Play]
  , trump :: Trump
  } deriving (Show, Eq, Ord)

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
