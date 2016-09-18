{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Card
import System.Environment 
import Data.List
import Control.Monad
import Control.Monad.Trans (lift, liftIO)
import Web.Scotty
import Data.Text.Lazy (pack, unpack)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')

main :: IO ()
main = do
  game <- newIORef $ nullGame
  serve game

-- API:
-- To start a new game, POST to /game
-- passing player names as a parameter
serve :: IORef Game -> IO ()
serve game = scotty 3000 $ do
  get "/card/:card" $ param "card" >>= (json. toCard)
  get "/game" $ liftIO (readIORef game) >>= json
  post "/game" $ do
    playerNames <- param "players" :: ActionM String
    let players = words playerNames
    newGame <- liftIO $ writeIORef game (Game [] [])
    json players

  post "/player" $ do
    name <- param "name"
    let player = Player name
    newGame <- liftIO $ atomicModifyIORef' game (\g -> ((addPlayer player g), (addPlayer player g)))
    json newGame
  post "/play" $ do
    cardString <- param "card"
    playerString <- param "player"
    let player = Player playerString
    case toCard cardString of
      Just card -> do
        let play = Play card player
        newGame <- liftIO $ atomicModifyIORef' game (\g -> ((addPlay play g), (addPlay play g)))
        json newGame

addPlayer :: Player -> Game -> Game
-- for now, we make a fake hand for the player
addPlayer p (Game rounds players) = Game rounds (p:players)

addPlay :: Play -> Game -> Game
-- what do we do when there are no rounds at this point?
-- for now we just no-op
addPlay play game@(Game [] players) = game
addPlay play game@(Game ((Round [] _):rs) players) = game
addPlay play (Game (round@(Round (trick:ts) hs):rs) players) = Game (newRound:rs) players
  where newRound = Round (newTrick:ts) hs
        newTrick = makePlay trick hand play
        hand = currentHand (getPlayPlayer play) round

nullGame :: Game
nullGame = Game [] []

newGame :: [Player] -> Game
newGame players = Game [round] players
  where round = Round [] hands
        cards = deal 7 (length players)
        hands = [] -- TODO: implement this

deal size number = deal_ size number [] shuffledDeck
deal_ size 0 acc deck = acc
deal_ size number acc deck = deal_ size (number - 1) ((take size deck):acc) (drop size deck)

