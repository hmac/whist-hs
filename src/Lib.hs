{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.Environment
import Control.Monad
import Data.List
import Data.Aeson

import Card
import Deck

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
  , getRoundDeck :: Deck
  } deriving (Show)

data Game = Game
  { rounds :: [Round]
  , players :: [Player]
  } deriving (Show)

data Player = Player { getName :: String} deriving (Show, Eq)

data Hand = Hand Player [Card] deriving (Show)

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

instance ToJSON Player where
  toJSON (Player name) = object [ "type" .= ("player" :: String)
                                , "name" .= name
                                ]

instance FromJSON Player where
  parseJSON (Object o) = Player <$> o .: "name"

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

instance FromJSON Play where
  parseJSON (Object o) = Play <$> o .: "card" <*> o .: "player"

instance ToJSON Trick where
  toJSON (Trick plays trump) = object [ "type" .= ("trick" :: String)
                                      , "trump" .= trump
                                      , "plays" .= map toJSON plays
                                      ]

instance ToJSON Round where
  toJSON (Round tricks hands deck) = object [ "type" .= ("round" :: String)
                                            , "tricks" .= tricks
                                            , "hands" .= hands
                                            , "deck" .= deck
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
        plays = concatMap getTrickPlays $ getRoundTricks round
        forPlayer (Play _ p) = p == player

-- Filters a set of Plays into only those that have the specified suit
filterSuit :: Suit -> [Play] -> [Play]
filterSuit suit = filter (\ (Play (Card s _) _) -> s == suit)

-- Determines if a Play is valid.
-- Checks that the card is in the player's hand
-- Checks that the player is following suit if they can
-- Checks that the player is trumping if they have trumps and can't follow suit
validPlay :: Play -> Hand -> Trump -> [Play] -> Bool
validPlay (Play card _) (Hand _ hand) trumps cardsPlayed
  | not (any (elem card) [hand]) = False
  | canFollowSuit && not followingSuit = False
  | not canFollowSuit && hasTrumps && not trump = False
  | otherwise = True
  where trump = suit card == trumps
        hasTrumps = any (\c -> suit c == trumps) hand
        suitLed = suit (getPlayCard $ head cardsPlayed)
        isLeading = null cardsPlayed
        followingSuit = isLeading || suit card == suitLed
        canFollowSuit = isLeading || any (\c -> suit c == suitLed) hand

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
  | player `elem` map (\(Play _ p) -> p) plays = trick
  | not (validPlay play hand trump plays) = trick
makePlay (Trick plays trump) _ play = Trick (play:plays) trump
