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
  game <- newIORef $ Game { rounds = [], players = [] }
  serve game

serve :: IORef Game -> IO ()
serve game = scotty 3000 $ do
  get "/card/:card" $ param "card" >>= (json. toCard)
  get "/game" $ liftIO (readIORef game) >>= json

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
