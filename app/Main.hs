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

data LocationName = Kitchen | Bedroom deriving (Show, Ord, Eq)

data Location = Location
  { desc :: String
  }
  deriving (Show)

locationInput = Map.fromList [("kitchen", Kitchen), ("bedroom", Bedroom)]

data GameState = GameState
  { language :: Language,
    locations :: Map LocationName Location,
    command :: Command,
    currentLocation :: LocationName
  }
  deriving (Show)

gameStateNewCommand :: GameState -> Command -> GameState
gameStateNewCommand ogs cmd =
  GameState
    { language = language ogs,
      locations = locations ogs,
      command = cmd,
      currentLocation = currentLocation ogs
    }

availableLocations :: Map LocationName Location -> [LocationName]
availableLocations m = map fst $ toList m

-- Commands
-- Set Language preference
-- go to location (relative or absolute or both?)
-- examine objects
data Command = ViewLocations | NoOp | Invalid | Quit | ViewGameState | GoTo LocationName deriving (Show, Eq)

quitWords = ["quit", "exit", "q"]

parseCommand :: String -> Command
parseCommand input
  | input `elem` quitWords = Quit
  | input == "locations" = ViewLocations
  | input == "gs" = ViewGameState
  | head (words input) == "go" = GoTo (locationInput Map.! last (words input))
  | otherwise = Invalid

locales = Map.fromList [(Kitchen, Location "Place in house"), (Bedroom, Location "Place where you sleep")]

main :: IO ()
main = do
  putStrLn "Yo"
  loop' GameState {language = English, locations = locales, command = NoOp, currentLocation = Bedroom}

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

eval' :: Command -> GameState -> GameState
eval' (GoTo newLocation) gs = GameState {language = language gs, locations = locations gs, currentLocation = newLocation, command = GoTo newLocation}
eval' cmd gs = gameStateNewCommand gs cmd

print' :: GameState -> IO ()
print' GameState {command = ViewLocations, locations = l} = print $ availableLocations l
print' GameState {command = GoTo newLocation, locations = l, currentLocation = cl} = print $ "You enter " ++ show newLocation ++ ". " ++ desc (l Map.! newLocation)
print' gs
  | command gs == ViewGameState = print gs
  | otherwise = print $ command gs

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'
  let cmd = parseCommand input

  unless
    (cmd == Quit)
    ( do
        let newGameState =  eval' cmd gs
        print' newGameState
        loop' newGameState
    )
