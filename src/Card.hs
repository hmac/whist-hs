{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Card 
  -- ( Suit
  -- , Card (Card)
  -- , toCard
  -- , toSuit
  -- , suit
     -- )
       where

import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T

data Suit   = Spade | Heart | Diamond | Club deriving (Show, Read, Eq, Enum, Bounded, Ord)
data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

data Card   = Card Suit Number deriving (Show, Read, Eq, Ord)

textNumberMap :: [(T.Text, Number)]
textNumberMap =  [("two", Two)
            ,("three", Three)
            ,("four", Four)
            ,("five", Five)
            ,("six", Six)
            ,("seven", Seven)
            ,("eight", Eight)
            ,("nine", Nine)
            ,("ten", Ten)
            ,("jack", Jack)
            ,("queen", Queen)
            ,("king", King)
            ,("ace", Ace)
            ]

textSuitMap :: [(T.Text, Suit)]
textSuitMap = [
            ("club", Club),
            ("diamond", Diamond),
            ("heart", Heart),
            ("spade", Spade)
           ]

invert = map (\(k, v) -> (v, k))
numberTextMap = invert textNumberMap
suitTextMap = invert textSuitMap

suit :: Card -> Suit
suit (Card suit _) = suit

instance ToJSON Suit where
  toJSON suit = case lookup suit suitTextMap of
                  Just s -> String s

instance ToJSON Number where
  toJSON number = case lookup number numberTextMap of
                    Just s -> String s

instance ToJSON Card where
  toJSON (Card suit number) = object [ "type" .= ("card" :: String)
                                     , "suit" .= suit
                                     , "number" .= number
                                     ]

instance FromJSON Suit where
  parseJSON (String s) = (pure . fromJust) $ lookup s textSuitMap
  parseJSON _ = mzero

instance FromJSON Number where
  parseJSON (String s) = (pure . fromJust) $ lookup s textNumberMap
  parseJSON _ = mzero

instance FromJSON Card where
  parseJSON (Object o) = Card <$> o .: "suit" <*> o .: "number"
