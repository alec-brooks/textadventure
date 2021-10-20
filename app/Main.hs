module Main where

import Control.Monad (unless)
import Data.Map (Map, toList)
import qualified Data.Map.Strict as Map
import System.IO

-- Game State
-- Language Preference
-- Locations (Locations have objects)
-- Objects

data Language = Japanese | English deriving (Show)

data Location = Location
  { desc :: String
  }
  deriving (Show)

data GameState = GameState
  { language :: Language,
    locations :: Map String Location,
    command :: Command
  }
  deriving (Show)

gameStateNewCommand :: GameState -> Command -> GameState
gameStateNewCommand ogs cmd =
  GameState
    { language = language ogs,
      locations = locations ogs,
      command = cmd
    }

availableLocations :: Map String Location -> [String]
availableLocations m = map fst $ toList m

-- Commands
-- Set Language preference
-- go to location (relative or absolute or both?)
-- examine objects
data Command = ViewLocations | NoOp | Invalid | Quit deriving (Show, Eq)

quitWords = ["quit", "exit", "q"]

parseCommand :: String -> Command
parseCommand input
  | input `elem` quitWords = Quit
  | input == "locations" = ViewLocations
  | otherwise = Invalid

locales = Map.fromList [("Kitchen", Location "Place in house"), ("Bedroom", Location "Place where you sleep")]

main :: IO ()
main = do
  putStrLn "Yo"
  loop' GameState {language = English, locations = locales, command = NoOp}

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

eval' :: Command -> GameState -> GameState
eval' cmd gs = gameStateNewCommand gs cmd

print' :: GameState -> IO ()
print' GameState {command = ViewLocations, locations = l} = print $ availableLocations l
print' GameState {command = c} = print c

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'
  let cmd = parseCommand input

  unless (cmd == Quit) $
    print' (eval' cmd gs) >> loop' gs
