{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Card
import Deck (deal, shuffledDeck)
import System.Environment 
import Data.List
import Control.Monad
import Control.Monad.Trans (lift, liftIO)
import Web.Scotty
import Data.Text.Lazy (pack, unpack)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')

main :: IO ()
main = do
  game <- newIORef nullGame
  serve game

-- API:
-- To start a new game, POST to /game
-- passing player names as a parameter
serve :: IORef Game -> IO ()
serve game = scotty 3000 $ do
  get "/game" $ liftIO (readIORef game) >>= json
  post "/game" $ do
    playerNames <- param "players" :: ActionM String
    let players = map Player $ words playerNames
    updatedGame <- liftIO $ newGame players
    savedGame <- liftIO $ writeIORef game updatedGame
    json updatedGame

  post "/player" $ do
    name <- param "name"
    let player = Player name
    newGame <- liftIO $ atomicModifyIORef' game (\g -> ((addPlayer player g), (addPlayer player g)))
    json newGame

  post "/play" $ do
    play <- jsonData
    newGame <- liftIO $ atomicModifyIORef' game (\g -> ((addPlay play g), (addPlay play g)))
    json newGame

addPlayer :: Player -> Game -> Game
-- for now, we make a fake hand for the player
addPlayer p (Game rounds players) = Game rounds (p:players)

addPlay :: Play -> Game -> Game
-- what do we do when there are no rounds at this point?
-- for now we just no-op
addPlay play game@(Game [] players) = game
addPlay play game@(Game (Round [] _ _ : rs) players) = game
addPlay play (Game (round@(Round (trick:ts) hs deck):rs) players) = Game (newRound:rs) players
  where newRound = Round (newTrick:ts) hs deck
        newTrick = makePlay trick hand play
        hand = currentHand (getPlayPlayer play) round

nullGame :: Game
nullGame = Game [] []

newGame :: [Player] -> IO Game
newGame players = do
  (deck, cardsForHands) <- deal 7 (length players) <$> shuffledDeck
  let hands = zipWith Hand players cardsForHands
  let round = Round [] hands deck
  return $ Game [round] players

