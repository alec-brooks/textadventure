module Main where
import Game (loop')
import GameObjects (startingGS)
import FormatMessage (wrap)
import State
import Data.Map

initialLocationText :: String
initialLocationText = locationDesc ( locations startingGS ! currentLocation startingGS) ! language startingGS

startMessage = "Welcome to text adventure. Type help for help\n" ++ wrap initialLocationText

main :: IO ()
main = do
  putStrLn startMessage
  loop' startingGS
