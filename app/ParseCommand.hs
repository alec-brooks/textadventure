module ParseCommand where
import State

import Data.Map
import Data.Char (toLower)

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
    Use (itemInput ! secondWord input) (objectInput ! lastWord input)
  | otherwise = Invalid

quitWords = ["quit", "exit", "q"]

inventoryWords = ["inventory", "i"]

interactWords = ["pick", "open", "interact"]

examineWords = ["examine", "look", "inspect"]

useJoiningWords = ["with", "on"]

useWords = ["use"]

goWords = ["go", "enter"]

lookupInput :: String -> Map String a -> (a -> Command) -> Command
lookupInput w input c = maybe Invalid c (input !? w)

lowerString :: String -> String
lowerString str = [toLower loweredString | loweredString <- str]

lastWord :: String -> String
lastWord = lowerString . last . words

secondWord :: String -> String
secondWord = lowerString . head . tail . words

locationInput = fromList [("kitchen", Kitchen), ("bedroom", Bedroom)]

objectInput = fromList [("book", Book), ("tome", Book), ("bed", Bed), ("door", Door)]

itemInput = fromList [("key", Key)]

