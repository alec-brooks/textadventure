module State where

import Data.Map (Map)
import Data.Set (Set)
import Text.Show.Functions

data Language = Japanese | English deriving (Show, Ord, Eq)

data ItemName = Key | Knife deriving (Show, Ord, Eq)

type Description = Map Language String

newtype Item = Item {itemDesc :: Description}

data LocationName = Kitchen | Bedroom deriving (Show, Ord, Eq)

data Location = Location
  { locationDesc :: Description,
    objects :: Map ObjectName Object
  }
  deriving (Show)

data ObjectName
  = Book
  | Bed
  | Door
  | Walls
  | BedsideTable
  | Fridge
  deriving (Show, Ord, Eq)

data Object = Object
  { objDesc :: Description,
    item :: Maybe ItemName,
    interactText :: Description,
    useItem :: Maybe ItemName,
    useText :: Description,
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
  | Help
  | ViewGameState
  | GoTo LocationName
  | Interact ObjectName
  | Examine ObjectName
  | Inventory
  | Look
  | Use ItemName ObjectName
  deriving (Show, Eq)
