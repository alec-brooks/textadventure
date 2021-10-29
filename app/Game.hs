module Game where

import Control.Monad (unless)
import Data.Char (toLower)
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
    item :: Item,
    interactText :: String
  }
  deriving (Show, Eq)

data Location = Location
  { locationDesc :: String,
    objects :: Map ObjectName Object
  }
  deriving (Show)

locationInput = Map.fromList [("kitchen", Kitchen), ("bedroom", Bedroom)]

objectInput = Map.fromList [("book", Book)]

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
data Command
  = ViewLocations
  | NoOp
  | Invalid
  | Quit
  | ViewGameState
  | GoTo LocationName
  | Interact ObjectName
  | Examine ObjectName
  deriving (Show, Eq)

quitWords = ["quit", "exit", "q"]

interactWords = ["pick", "open", "interact"]

examineWords = ["examine", "look"]

-- dry these out
lookupLocation :: String -> Command
lookupLocation locationWord =
  maybe
    Invalid
    GoTo
    (locationInput Map.!? locationWord)

lookupObject :: String -> Command
lookupObject objectWord =
  maybe
    Invalid
    Interact
    (objectInput Map.!? objectWord)

lookupExamineObject :: String -> Command
lookupExamineObject objectWord =
  maybe
    Invalid
    Examine
    (objectInput Map.!? objectWord)

lowerString :: String -> String
lowerString str = [toLower loweredString | loweredString <- str]

parseCommand :: String -> Command
parseCommand input
  | input `elem` quitWords = Quit
  | input == "locations" = ViewLocations
  | input == "gs" = ViewGameState
  | head (words input) == "go" = lookupLocation $ lowerString $ last $ words input
  | head (words input) `elem` interactWords = lookupObject $ lowerString $ last $ words input
  | head (words input) `elem` examineWords = lookupExamineObject $ lowerString $ last $ words input
  | otherwise = Invalid

book =
  Object
    { objDesc = "An old tome, it smells like glue",
      item = Key,
      interactText = "Inside the book is a hollowed out chamber containing a key"
    }

locales :: Map LocationName Location
locales =
  Map.fromList
    [ ( Kitchen,
        Location "Place in house" Map.empty
      ),
      ( Bedroom,
        Location "Place where you sleep. There is a book lying on the bedside table." $ Map.fromList [(Book, book)]
      )
    ]

startingGS = GameState {language = English, locations = locales, command = NoOp, currentLocation = Bedroom}

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

eval' :: Command -> GameState -> GameState
eval' (GoTo newLocation) gs = GameState {language = language gs, locations = locations gs, currentLocation = newLocation, command = GoTo newLocation}
eval' (Interact object) gs = GameState {language = language gs, locations = locations gs, currentLocation = currentLocation gs, command = Interact object}
eval' cmd gs = gameStateNewCommand gs cmd

formatMessage :: GameState -> String
formatMessage GameState {command = ViewLocations, locations = l} = show $ availableLocations l
formatMessage GameState {command = GoTo newLocation, locations = l, currentLocation = cl} = "You enter " ++ show newLocation ++ ". " ++ locationDesc (l Map.! newLocation)
formatMessage GameState {command = Examine o, locations = l, currentLocation = cl} = do
  let os = objects $ l Map.! cl
  objDesc $ os Map.! o
formatMessage GameState {command = Interact o, locations = l, currentLocation = cl} = do
  let os = objects $ l Map.! cl
  interactText $ os Map.! o
formatMessage gs
  | command gs == ViewGameState = show gs
  | otherwise = show $ command gs

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'
  let cmd = parseCommand input

  unless
    (cmd == Quit)
    ( do
        let newGameState = eval' cmd gs
        print $ formatMessage newGameState
        loop' newGameState
    )