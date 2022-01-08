module State where

import Data.Map (Map)
import Data.Set (Set)
import Text.Show.Functions

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
