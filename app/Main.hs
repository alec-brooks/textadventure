module Main where
import Game (loop')
import GameObjects (startingGS)
import State
import Data.Map

initialLocationText :: String
initialLocationText = locationDesc $ locations startingGS ! currentLocation startingGS

main :: IO ()
main = do
  putStrLn initialLocationText
  loop' startingGS
