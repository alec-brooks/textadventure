module FormatMessage where

import Data.Map
import State

formatMessage :: GameState -> String
formatMessage GameState {gameError = Just e} = show e
formatMessage GameState {command = ViewLocations, navigableLocations = l} = show l
formatMessage GameState {command = GoTo newLocation, locations = l, currentLocation = cl} = "You enter " ++ show newLocation ++ ". " ++ locationDesc (l ! newLocation)
formatMessage GameState {command = Look, locations = l, currentLocation = cl} = locationDesc $ l ! cl
formatMessage GameState {command = Examine o, locations = l, currentLocation = cl} = do
  let os = objects $ l ! cl
  objDesc $ os ! o
formatMessage GameState {command = Interact o, locations = l, currentLocation = cl} = do
  let os = objects $ l ! cl
  let object = os ! o
  case item object of
    Nothing -> interactText object
    Just item -> interactText object ++ "\n" ++ show item ++ " added to inventory"
formatMessage gs
  | command gs == ViewGameState = show gs
  | command gs == Inventory = show $ inventory gs
  | otherwise = show $ command gs
