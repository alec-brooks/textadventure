module Game where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Map (Map, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import System.IO
import Text.Show.Functions

-- todo:
--   use items on objects
--   refactor
--   make initial game state layout nicer
--
--
--   help h
--   proper context printed at the start
--   pretty print inventory

data Language = Japanese | English deriving (Show)

data ItemName = Key deriving (Show, Ord, Eq)

data Item = Item
  { itemDesc :: String,
    interaction :: Object -> GameState -> GameState
  }

data LocationName = Kitchen | Bedroom deriving (Show, Ord, Eq)

data Location = Location
  { locationDesc :: String,
    objects :: Map ObjectName Object
  }
  deriving (Show)

data ObjectName = Book | Bed | Door deriving (Show, Ord, Eq)

data Object = Object
  { objDesc :: String,
    item :: Maybe ItemName,
    interactText :: String,
    useItem :: Maybe ItemName,
    useText :: String,
    useFn :: GameState -> GameState
  }
  deriving (Show)

data Error = UnableToGo | NoItem | NoObject deriving (Show)

locationInput = Map.fromList [("kitchen", Kitchen), ("bedroom", Bedroom)]

objectInput = Map.fromList [("book", Book), ("tome", Book), ("bed", Bed), ("door", Door)]

itemInput = Map.fromList [("key", Key)]

data GameState = GameState
  { language :: Language,
    locations :: Map LocationName Location,
    command :: Command,
    currentLocation :: LocationName,
    inventory :: Set ItemName,
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
  | Use ItemName ObjectName
  deriving (Show, Eq)

quitWords = ["quit", "exit", "q"]

inventoryWords = ["inventory", "i"]

interactWords = ["pick", "open", "interact"]

examineWords = ["examine", "look", "inspect"]

useJoiningWords = ["with", "on"]

useWords = ["use"]

goWords = ["go", "enter"]

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

secondWord :: String -> String
secondWord = lowerString . head . tail . words

parseCommand :: String -> Command
parseCommand input
  | input `elem` quitWords = Quit
  | input == "locations" = ViewLocations
  | input == "gs" = ViewGameState
  | input `elem` inventoryWords = Inventory
  | input `elem` examineWords = Look
  | head (words input) `elem` goWords = lookupInput (lastWord input) locationInput GoTo
  | head (words input) `elem` interactWords = lookupInput (lastWord input) objectInput Interact
  | head (words input) `elem` examineWords = lookupInput (lastWord input) objectInput Examine
  -- This needs to be safer
  | any (`elem` useJoiningWords) (words input) && head (words input) `elem` useWords =
    Use (itemInput Map.! secondWord input) (objectInput Map.! lastWord input)
  | otherwise = Invalid

defaultObj =
  Object
    { objDesc = "It's an object",
      item = Nothing,
      interactText = "You can't seem to do anything with this",
      useItem = Nothing,
      useText = "You can't seem to do anything with this",
      useFn = id
    }

book =
  defaultObj
    { objDesc = "An old tome, it smells like glue",
      item = Just Key,
      interactText = "Inside the book is a hollowed out chamber containing a key"
    }

bed =
  defaultObj
    { objDesc = "A normal old bed",
      item = Nothing
    }

door =
  defaultObj
    { objDesc = "A sorry looking wooden door. There is a key hole present near the handle",
      item = Nothing,
      interactText = "Despite it's frail appearance you can't get it to budge",
      useItem = Just Key,
      useText = "The key fits the lock and after some effort the door swings open, revealing a path to the kitchen",
      useFn = \gs -> gs {navigableLocations = Set.insert Kitchen $ navigableLocations gs}
    }

locales :: Map LocationName Location
locales =
  Map.fromList
    [ ( Kitchen,
        Location "An old kitchen. The walls are stone and the air is cool" Map.empty
      ),
      ( Bedroom,
        Location "The room where you rest. There is a book lying on the bedside table. A frail door stands in the middle of one of the blank walls" $ Map.fromList [(Book, book), (Bed, bed), (Door, door)]
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

objectNotInRoom :: ObjectName -> Location -> Bool
objectNotInRoom o location = Map.notMember o $ objects location

getObject :: ObjectName -> GameState -> Maybe Object
getObject o gs = Map.lookup o $ objects l
  where
    l = locations gs Map.! currentLocation gs

getUseFn :: GameState -> Maybe Object -> GameState -> GameState
getUseFn gs = Maybe.maybe (\g -> g {gameError = Just NoObject}) useFn

useItemObject :: ItemName -> ObjectName -> GameState -> GameState
useItemObject item object gs
  | Set.notMember item (inventory gs) = gs {gameError = Just NoItem}
  | Maybe.isNothing maybeObj = gs {gameError = Just NoObject}
  | otherwise = getUseFn gs maybeObj gs {command = Use item object, gameError = Nothing}
  where
    maybeObj = getObject object gs

eval' :: Command -> GameState -> GameState
eval' (GoTo newLocation) gs =
  if newLocation `elem` navigableLocations gs
    then
      gs
        { currentLocation = newLocation,
          command = GoTo newLocation,
          gameError = Nothing
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
          Just item -> Set.insert item $ inventory gs,
      gameError = Nothing
    }
eval' (Use item object) gs = useItemObject item object gs
eval' cmd gs = gs {command = cmd, gameError = Nothing}

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
