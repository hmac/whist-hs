{-# LANGUAGE FlexibleInstances #-}

module Card 
  ( Suit
  , Card (Card)
  , toCard
  , toSuit
  , suit
  ) where

import Control.Monad

data Suit   = Spade | Heart | Diamond | Club deriving (Show, Read, Eq, Enum, Bounded, Ord)
data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

data Card   = Card Suit Number deriving (Show, Read, Eq, Ord)

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

suitList :: [(Char, Suit)]
suitList = [
            ('C', Club),
            ('D', Diamond),
            ('H', Heart),
            ('S', Spade)
           ]

toCard :: String -> Maybe Card
toCard (s:n) = liftM2 Card (toSuit s) (lookup n numberList)

suit :: Card -> Suit
suit (Card suit _) = suit

class ToSuit a where
  toSuit :: a -> Maybe Suit

instance ToSuit String where
  toSuit s = lookup (head s) suitList

instance ToSuit Char where
  toSuit c = lookup c suitList
