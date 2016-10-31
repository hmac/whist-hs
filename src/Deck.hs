{-# LANGUAGE OverloadedStrings #-}
module Deck where

import Card
import System.Random.Shuffle
import System.Random (getStdRandom)
import Data.Aeson

data Deck = Deck [Card]
  deriving Show

numbers = [
  Two,
  Three,
  Four,
  Five,
  Six,
  Seven,
  Eight,
  Nine,
  Ten,
  Jack,
  Queen,
  King,
  Ace
  ]

suits = [
  Club,
  Diamond,
  Heart,
  Spade
  ]

allCards :: [Card]
allCards = concatMap (\s -> map (Card s) numbers) suits

shuffledDeck :: IO Deck
shuffledDeck = Deck <$> shuffleM allCards

deal :: Int -> Int -> Deck -> (Deck, [[Card]])
deal size number (Deck cards) = (Deck rest, dealt)
  where (rest, dealt) = deal_ size number cards []

deal_ :: Int -> Int -> [Card] -> [[Card]] -> ([Card], [[Card]])
deal_ size 0 deck acc = (deck, acc)
deal_ size number deck acc = deal_ size (number - 1) rest (dealt : acc)
  where (dealt, rest) = splitAt size deck

