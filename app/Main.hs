module Main where

import Control.Monad (unless)
import Data.Map (Map, toList)
import qualified Data.Map.Strict as Map
import System.IO

-- Game State
-- Language Preference
-- Locations (Locations have objects)
-- Objects


-- todo: tests that take string input and game state and assert on game state
--       interact with objects in room
--       make initial game state layout nicer

data Language = Japanese | English deriving (Show)

data LocationName = Kitchen | Bedroom deriving (Show, Ord, Eq)

data ObjectName = Book deriving (Show, Ord, Eq)

data Item = Key deriving (Show, Ord, Eq)

data Object = Object
  { objDesc :: String,
    item :: Item
  }
  deriving (Show)

data Location = Location
  { locationDesc :: String,
    objects :: Map ObjectName Object
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

locales :: Map LocationName Location
locales =
  Map.fromList
    [ ( Kitchen,
        Location "Place in house" Map.empty
      ),
      ( Bedroom,
        Location "Place where you sleep. There is a book lying on the bedside table." $ Map.fromList [(Book, Object "An old tome, it smells like glue" Key)]
      )
    ]

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
print' GameState {command = GoTo newLocation, locations = l, currentLocation = cl} = print $ "You enter " ++ show newLocation ++ ". " ++ locationDesc (l Map.! newLocation)
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
        let newGameState = eval' cmd gs
        print' newGameState
        loop' newGameState
    )
