module Main where

import Lib
import System.Environment 
import Data.List
import Control.Monad

main :: IO ()
main = do
  args <- getArgs

  let sCard = extractArgs args "-p"
  let sHand = extractArgs args "-h"
  let sTrumps = extractArgs args "-t"
  let sCardLed = extractArgs args "-l"
  let playerName = extractArgs args "-n"

  -- TODO: handle arguments not being given at runtime
  let player = Player (head playerName)
  let card = readCard (head sCard)
  let hand = fmap (Hand player) (mapM readCard sHand)
  let trumps = readSuit (head (head sTrumps))
  let cardLed = readCard (head sCardLed)
  let play = fmap (\c -> Play c player) card
  let result = liftM4 validPlay play hand trumps cardLed

  case result of
    Just b -> (putStrLn . show) b
    otherwise -> putStrLn "error"

extractArgs :: [String] -> String -> [String]
extractArgs args opt 
  | (head args) == opt = takeWhile (\s -> not $ isPrefixOf "-" s) (tail args)
  | length args == 0 = []
  | otherwise = extractArgs (drop 1 args) opt
