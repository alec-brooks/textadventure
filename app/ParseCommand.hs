module ParseCommand where

import Data.Char (toLower)
import Data.Map (Map, fromList, (!), (!?))
import State
import Data.Maybe (isNothing, fromJust)

parseCommand :: String -> Command
parseCommand input
  | input `elem` quitWords = Quit
  | input == "locations" = ViewLocations
  | input == "gs" = ViewGameState
  | input `elem` inventoryWords = Inventory
  | input `elem` examineWords = Look
  | input `elem` helpWords = Help
  | head processedWords `elem` goWords = lookupInput (lastWord processedWords) locationInput GoTo
  | head processedWords `elem` interactWords = lookupInput (lastWord processedWords) objectInput Interact
  | head processedWords `elem` examineWords = lookupInput (lastWord processedWords) objectInput Examine
  | any (`elem` useJoiningWords) processedWords && head processedWords `elem` useWords = useInput processedWords
  | otherwise = Invalid
  where
    processedWords = filter (/= "the") $ words input

quitWords = ["quit", "exit", "q"]

inventoryWords = ["inventory", "i"]

interactWords = ["pick", "open", "interact"]

examineWords = ["examine", "look", "inspect", "check"]

useJoiningWords = ["with", "on", "in"]

useWords = ["use", "put"]

goWords = ["go", "enter"]

helpWords = ["h", "help"]

lookupInput :: String -> Map String a -> (a -> Command) -> Command
lookupInput w input c = maybe Invalid c (input !? w)

useInput :: [String] -> Command
useInput wds = if isNothing item || isNothing obj then
    Invalid
  else
    Use (fromJust item) (fromJust obj)
  where
    item = itemInput !? secondWord wds
    obj = objectInput !? lastWord wds

lowerString :: String -> String
lowerString str = [toLower loweredString | loweredString <- str]

lastWord :: [String] -> String
lastWord = lowerString . last

secondWord :: [String] -> String
secondWord = lowerString . head . tail

locationInput = fromList [("kitchen", Kitchen), ("bedroom", Bedroom)]

objectInput =
  fromList
    [ ("book", Book),
      ("tome", Book),
      ("bed", Bed),
      ("door", Door),
      ("walls", Walls),
      ("table", BedsideTable)
    ]

itemInput = fromList [("key", Key)]
