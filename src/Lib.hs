module Lib where

import System.Environment 
import Control.Monad
import Data.List

data Card   = Card Suit Number deriving (Show, Read, Eq, Ord)
data Suit   = Spade | Heart | Diamond | Club deriving (Show, Read, Eq, Enum, Bounded, Ord)
data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

-- A Trump is just a particular suit
type Trump = Suit

-- A Play is a card played by a specific person
-- TODO: embed the trump suit into Play
data Play = Play Card Player deriving (Show, Eq)

instance Ord Play where
  compare (Play c1 _) (Play c2 _) = compare c1 c2

-- A Player represents someone playing the game
data Player = Player String deriving (Show, Eq)

-- A Hand represents a set of cards belonging to one player (their 'hand')
data Hand = Hand Player [Card]
  deriving (Show)

-- Lookup table for numbers
-- Uses String because "10" is two chars
numberList :: [(String, Number)]
numberList =  [("2", Two)
            ,("3", Three)
            ,("4", Four)
            ,("5", Five)
            ,("6", Six)
            ,("7", Seven)
            ,("8", Eight)
            ,("9", Nine)
            ,("10", Ten)
            ,("J", Jack)
            ,("Q", Queen)
            ,("K", King)
            ,("A", Ace)
            ]

-- Lookup table for suits
suitList :: [(Char, Suit)]
suitList = [
            ('C', Club),
            ('D', Diamond),
            ('H', Heart),
            ('S', Spade)
           ]

readNum :: String -> Maybe Number
readNum s = lookup s numberList

readSuit :: Char -> Maybe Suit
readSuit s = lookup s suitList

readCard :: String -> Maybe Card
readCard (s:n) = liftM2 Card (readSuit s) (readNum n)

suit :: Card -> Suit
suit (Card suit _) = suit

-- TODO: make nicer
cardFromPlay :: Play -> Card
cardFromPlay (Play card player) = card

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
  | and [ (not canFollowSuit), (not trump), hasTrumps ] = False
  | otherwise = True
  where trump = (suit card) == trumps
        hasTrumps = any (\c -> (suit c) == trumps) hand
        suitLed = suit (cardFromPlay $ head cardsPlayed)
        isLeading = null cardsPlayed
        followingSuit = isLeading || (suit card) == suitLed
        canFollowSuit = isLeading || any (\c -> (suit c) == suitLed) hand
