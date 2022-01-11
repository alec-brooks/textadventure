module FormatMessage where

import Data.List (elemIndex, intercalate, intersperse)
import Data.List.Split
import Data.Map (Map, (!))
import Data.Maybe
import qualified Data.Set as Set
import State

replaceAtIndex :: [a] -> [a] -> Int -> [a]
replaceAtIndex r l i = do
  let (x, _ : ys) = splitAt i l
  x ++ r ++ ys

insertNewLineNearEnd :: String -> String
insertNewLineNearEnd s = do
  let spaceIndex = elemIndex ' ' rs
  reverse $ maybe rs (replaceAtIndex "\n" rs) spaceIndex
  where
    rs = reverse s

wrapText :: Int -> String -> String
wrapText n s = do
  let chunks = chunksOf n s
  concatMap insertNewLineNearEnd (init chunks) ++ last chunks

getObject :: Map LocationName Location -> LocationName -> ObjectName -> Object
getObject l cl = (!) $ objects (l ! cl)

showError :: Error -> String
showError UnableToGo = "That location is not accessible"
showError NoItem = "That item is not in your inventory"
showError NoObject = "There is no object like that in here"

formatMessage :: GameState -> String
formatMessage GameState {gameError = Just e} = wrap $ showError e
formatMessage GameState {command = ViewLocations, navigableLocations = l} = show l
formatMessage
  GameState
    { command = GoTo newLoc,
      locations = l,
      currentLocation = cl,
      language = lan
    } = wrap "You enter " ++ show newLoc ++ ". " ++ locationDesc (l ! newLoc) ! lan
formatMessage
  GameState
    { command = Look,
      locations = l,
      currentLocation = cl,
      language = lan
    } = wrap $ locationDesc (l ! cl) ! lan
formatMessage
  GameState
    { command = Examine o,
      locations = l,
      currentLocation = cl,
      language = lan
    } = wrap $ objDesc (getObject l cl o) ! lan
formatMessage
  GameState
    { command = Interact o,
      locations = l,
      currentLocation = cl,
      language = lan
    } = case item object of
    Nothing -> wrap iText
    Just item -> wrap iText ++ "\n" ++ show item ++ " added to inventory"
    where
      object = getObject l cl o
      iText = interactText object ! lan
formatMessage
  GameState
    { command = Use item objectName,
      locations = l,
      currentLocation = cl,
      language = lan
    } =
    wrap $ useText (getObject l cl objectName) ! lan
formatMessage gs
  | command gs == ViewGameState = show gs
  | command gs == Help = wrap helpText
  | command gs == Invalid = wrap invalidText
  | command gs == Inventory =
    "Inventory Contents:\n" ++ intercalate "\n" (map show . Set.toList $ inventory gs)
  | otherwise = show $ command gs

wrap = wrapText 80

helpText =
  "This is a game that is played using commands that you issue in order to explore and\
  \ interact with the world around you. Try using verbs like LOOK or INSPECT to find out\
  \ more about objects. Try to OPEN or INTERACT with objects. If you pick something up\
  \ and it is in your INVENTORY, then you can USE ITEM ON OBJECTS in order to progress."

invalidText = "That doesn't seem like a valid action"
