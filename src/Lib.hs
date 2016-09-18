{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.Environment
import Control.Monad
import Data.List
import Data.Aeson

import Card

type Trump = Suit

-- TODO: embed the trump suit into Play?
data Play = Play
  { getPlayCard :: Card
  , getPlayPlayer :: Player
  } deriving (Show, Eq)

data Trick = Trick
  { getTrickPlays :: [Play]
  , getTrickTrump :: Trump
  } deriving (Show, Eq, Ord)

data Round = Round
  { getRoundTricks :: [Trick]
  , getRoundHands :: [Hand]
  } deriving (Show)

data Game = Game
  { rounds :: [Round]
  , players :: [Player]
  } deriving (Show)

-- State changes:
-- Create game with set number of players
-- ROUND BEGIN
--  Choose dealer
--  Deal
--  Choose trumps
--  TRICK BEGIN
--   Players make plays
--  TRICK END
-- ROUND END
--
-- Maybe you only need three states:
-- round_start
-- choose_trumps
-- trick_start

instance Ord Play where
  compare (Play c1 _) (Play c2 _) = compare c1 c2

data Player = Player { getName :: String} deriving (Show, Eq)

data Hand = Hand Player [Card] deriving (Show)

instance ToJSON Suit where
  toJSON Spade = "spade"
  toJSON Heart = "heart"
  toJSON Diamond = "diamond"
  toJSON Club = "club"

instance ToJSON Number where
  toJSON Two = "two"
  toJSON Three = "three"
  toJSON Four = "four"
  toJSON Five = "five"
  toJSON Six = "six"
  toJSON Seven = "seven"
  toJSON Eight = "eight"
  toJSON Nine = "nine"
  toJSON Ten = "ten"
  toJSON Jack = "jack"
  toJSON Queen = "queen"
  toJSON King = "king"

instance ToJSON Card where
  toJSON (Card suit number) = object [ "type" .= ("card" :: String)
                                     , "suit" .= suit
                                     , "number" .= number
                                     ]

instance ToJSON Player where
  toJSON (Player name) = object [ "type" .= ("player" :: String)
                                , "name" .= name
                                ]

instance ToJSON Hand where
  toJSON (Hand player cards) = object [ "type" .= ("hand" :: String)
                                      , "player" .= player
                                      , "cards" .= cards
                                      ]

instance ToJSON Play where
  toJSON (Play card player) = object [ "type" .= ("play" :: String)
                                      , "card" .= card
                                      , "player" .= player
                                     ]

instance ToJSON Trick where
  toJSON (Trick plays trump) = object [ "type" .= ("trick" :: String)
                                      , "trump" .= trump
                                      , "plays" .= map toJSON plays
                                      ]

instance ToJSON Round where
  toJSON (Round tricks hands) = object [ "type" .= ("round" :: String)
                                       , "tricks" .= tricks
                                       , "hands" .= hands
                                       ]

instance ToJSON Game where
  toJSON (Game rounds players) = object [ "type" .= ("game" :: String)
                                        , "rounds" .= rounds
                                        , "players" .= players
                                        ]

-- the cards in a player's hand that they haven't played yet
-- assumes the player is in the game
currentHand :: Player -> Round -> Hand
currentHand player round = Hand player cardsLeft
  where cardsLeft = map getPlayCard . filter forPlayer $ plays
        plays = concat . (map getTrickPlays)$ (getRoundTricks round)
        forPlayer (Play _ p) = p == player

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
        suitLed = suit (getPlayCard $ head cardsPlayed)
        isLeading = null cardsPlayed
        followingSuit = isLeading || (suit card) == suitLed
        canFollowSuit = isLeading || any (\c -> (suit c) == suitLed) hand

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
