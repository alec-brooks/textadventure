module GameObjects where

import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import State

defaultObj =
  Object
    { objDesc = fromList [(English, "It's an object")],
      item = Nothing,
      interactText = fromList [(English, "You can't seem to do anything with this")],
      useItem = Nothing,
      useText = fromList [(English, "You can't seem to do anything with this")],
      useFn = id
    }

book =
  defaultObj
    { objDesc = fromList [(English, "An old tome, it smells like glue")],
      item = Just Key,
      interactText = fromList [(English, "Inside the book is a hollowed out chamber containing a key")]
    }

bed =
  defaultObj
    { objDesc = fromList [(English, "A normal old bed")],
      item = Nothing
    }

bedsideTable =
  defaultObj
    { objDesc = fromList [(English, "A plain bedside table. There are no drawers. On it lies the book")],
      item = Nothing
    }

walls =
  defaultObj
    { objDesc = fromList [(English, "Lightly scuffed but solid. They could be plastered brick")],
      item = Nothing
    }

door =
  defaultObj
    { objDesc = fromList [(English, "A sorry looking wooden door. There is a key hole present near the handle")],
      item = Nothing,
      interactText = fromList [(English, "Despite it's frail appearance you can't get it to budge")],
      useItem = Just Key,
      useText = fromList [(English, "The key fits the lock and after some effort the door swings open, revealing a path to the kitchen")],
      useFn = \gs -> gs {navigableLocations = Set.insert Kitchen $ navigableLocations gs}
    }

fridge =
  defaultObj
    { objDesc = fromList [(English, "Honking great fridge I tell you")],
      item = Just Knife,
      interactText = fromList [(English, "You crack open the door and find nothing but a rusty kitchen knife")]
    }

locales :: Map LocationName Location
locales =
  fromList
    [ ( Kitchen,
        Location kitchenDesc Map.empty
      ),
      ( Bedroom,
        Location bedroomDesc bedroomObjects
      )
    ]
  where
    kitchenDesc = fromList [(English, "An old kitchen. The walls are stone and the air is cool")]
    bedroomDesc = fromList [(English, "You are in the room where you rest. There is a book lying on the bedside table. A frail door stands in the middle of one of the blank walls")]
    bedroomObjects = fromList [(Book, book), (Bed, bed), (Door, door), (Walls, walls), (BedsideTable, bedsideTable)]
    kitchenObjects = fromList [(Fridge, fridge)]

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
