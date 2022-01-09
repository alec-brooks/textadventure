module GameObjects where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import State

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

bedsideTable =
  defaultObj
    { objDesc = "A plain bedside table. There are no drawers. On it lies the book",
      item = Nothing
    }

walls =
  defaultObj
    { objDesc = "Lightly scuffed but solid. They could be plastered brick",
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
        Location kitchenDesc Map.empty
      ),
      ( Bedroom,
        Location bedroomDesc bedroomObjects
      )
    ]
  where
    kitchenDesc = "An old kitchen. The walls are stone and the air is cool"
    bedroomDesc = "You are in the room where you rest. There is a book lying on the bedside table. A frail door stands in the middle of one of the blank walls"
    bedroomObjects = Map.fromList [(Book, book), (Bed, bed), (Door, door), (Walls, walls), (BedsideTable, bedsideTable)]

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
