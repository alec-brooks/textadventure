module Main where

import Data.Map
import FormatMessage (wrap)
import Game (loop')
import GameObjects (startingGS)
import State

initialLocationText :: String
initialLocationText = locationDesc currentLocale ! language startingGS
  where
    currentLocale = locations startingGS ! currentLocation startingGS

startMessage =
  "Welcome to text adventure. Type help for help\n"
    ++ wrap initialLocationText

main :: IO ()
main = do
  putStrLn startMessage
  loop' startingGS
