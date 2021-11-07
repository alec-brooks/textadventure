module Game where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Map (Map, toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

-- todo:
--   use items on objects
--   gate movement until condition is met
--   make initial game state layout nicer

data Language = Japanese | English deriving (Show)

data LocationName = Kitchen | Bedroom deriving (Show, Ord, Eq)

data ObjectName = Book | Bed | Door deriving (Show, Ord, Eq)

data Item = Key deriving (Show, Ord, Eq)

data Object = Object
  { objDesc :: String,
    item :: Maybe Item,
    interactText :: String
  }
  deriving (Show, Eq)

data Location = Location
  { locationDesc :: String,
    objects :: Map ObjectName Object
  }
  deriving (Show)

data Error = UnableToGo deriving (Show)

locationInput = Map.fromList [("kitchen", Kitchen), ("bedroom", Bedroom)]

objectInput = Map.fromList [("book", Book), ("tome", Book), ("bed", Bed), ("door", Door)]

data GameState = GameState
  { language :: Language,
    locations :: Map LocationName Location,
    command :: Command,
    currentLocation :: LocationName,
    inventory :: Set Item,
    navigableLocations :: Set LocationName,
    gameError :: Maybe Error
  }
  deriving (Show)

availableLocations :: Map LocationName Location -> [LocationName]
availableLocations m = map fst $ toList m

data Command
  = ViewLocations
  | NoOp
  | Invalid
  | Quit
  | ViewGameState
  | GoTo LocationName
  | Interact ObjectName
  | Examine ObjectName
  | Inventory
  | Look
  deriving (Show, Eq)

quitWords = ["quit", "exit", "q"]

inventoryWords = ["inventory", "i"]

interactWords = ["pick", "open", "interact"]

examineWords = ["examine", "look", "inspect"]

lookupInput :: String -> Map String a -> (a -> Command) -> Command
lookupInput w input c =
  maybe
    Invalid
    c
    (input Map.!? w)

lowerString :: String -> String
lowerString str = [toLower loweredString | loweredString <- str]

lastWord :: String -> String
lastWord = lowerString . last . words

parseCommand :: String -> Command
parseCommand input
  | input `elem` quitWords = Quit
  | input == "locations" = ViewLocations
  | input == "gs" = ViewGameState
  | input `elem` inventoryWords = Inventory
  | input `elem` examineWords = Look
  | head (words input) == "go" = lookupInput (lastWord input) locationInput GoTo
  | head (words input) `elem` interactWords = lookupInput (lastWord input) objectInput Interact
  | head (words input) `elem` examineWords = lookupInput (lastWord input) objectInput Examine
  | otherwise = Invalid

book =
  Object
    { objDesc = "An old tome, it smells like glue",
      item = Just Key,
      interactText = "Inside the book is a hollowed out chamber containing a key"
    }

bed =
  Object
    { objDesc = "A normal old bed",
      item = Nothing,
      interactText = "You can't seem to do anything with this"
    }

door =
  Object
    { objDesc = "A sorry looking wooden door. There is a key hole present near the handle",
      item = Nothing,
      interactText = "Despite it's frail appearance you can't get it to budge"
    }

locales :: Map LocationName Location
locales =
  Map.fromList
    [ ( Kitchen,
        Location "An old kitchen. The walls are stone and the air is cool" Map.empty
      ),
      ( Bedroom,
        Location "The room where you rest. There is a book lying on the bedside table." $ Map.fromList [(Book, book), (Bed, bed), (Door, door)]
      )
    ]

startingGS =
  GameState
    { language = English,
      locations = locales,
      command = NoOp,
      currentLocation = Bedroom,
      inventory = Set.empty,
      navigableLocations = Set.singleton Bedroom,
      gameError = Nothing
    }

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

eval' :: Command -> GameState -> GameState
eval' (GoTo newLocation) gs =
  if newLocation `elem` navigableLocations gs
    then
      gs
        { currentLocation = newLocation,
          command = GoTo newLocation
        }
    else gs {gameError = Just UnableToGo}
eval' (Interact object) gs =
  gs
    { command = Interact object,
      inventory = do
        let l = locations gs Map.! currentLocation gs
        let o = objects l Map.! object
        let i = item o
        case i of
          Nothing -> inventory gs
          Just item -> Set.insert item $ inventory gs
    }
eval' cmd gs = gs {command = cmd}

formatMessage :: GameState -> String
formatMessage GameState {gameError = Just e} = show e
formatMessage GameState {command = ViewLocations, locations = l} = show $ availableLocations l
formatMessage GameState {command = GoTo newLocation, locations = l, currentLocation = cl} = "You enter " ++ show newLocation ++ ". " ++ locationDesc (l Map.! newLocation)
formatMessage GameState {command = Look, locations = l, currentLocation = cl} = locationDesc $ l Map.! cl
formatMessage GameState {command = Examine o, locations = l, currentLocation = cl} = do
  let os = objects $ l Map.! cl
  objDesc $ os Map.! o
formatMessage GameState {command = Interact o, locations = l, currentLocation = cl} = do
  let os = objects $ l Map.! cl
  let object = os Map.! o
  case item object of
    Nothing -> interactText object
    Just item -> interactText object ++ "\n" ++ show item ++ " added to inventory"
formatMessage gs
  | command gs == ViewGameState = show gs
  | command gs == Inventory = show $ inventory gs
  | otherwise = show $ command gs

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'
  let cmd = parseCommand input

  unless
    (cmd == Quit)
    ( do
        let newGameState = eval' cmd gs
        putStrLn $ formatMessage newGameState
        loop' newGameState
    )
