module Main where

import Control.Monad (unless)
import Data.Map (Map, toList)
import qualified Data.Map.Strict as Map
import System.IO

-- Game State
-- Language Preference
-- Locations (Locations have objects)
-- Objects

data Language = Japanese | English deriving (Show)

data Location = Location
  { desc :: String
  } deriving (Show)

data Command = ViewLocations deriving (Show)

data GameState = GameState
  { language :: Language,
    locations :: Map String Location,
    command :: Command
  } deriving (Show)

gameStateNewCommand :: GameState -> Command -> GameState
gameStateNewCommand ogs cmd =
  GameState
    { language = language ogs,
      locations = locations ogs,
      command = cmd
    }

availableLocations :: Map String Location -> [String]
availableLocations m = map fst $ toList m

-- Commands
-- Set Language preference
-- go to location (relative or absolute or both?)
-- examine objects

locales = Map.fromList [("Kitchen", Location "Place in house"), ("Bedroom", Location "PLace where you sleep")]

main :: IO ()
main = do
  putStrLn "Yo"
  loop' GameState {language = English, locations = locales, command = ViewLocations}

read' :: IO String
read' =
  putStr "REPL> "
    >> hFlush stdout
    >> getLine

eval' :: String -> GameState -> GameState
eval' input gs =
  if input == "locations"
    then gameStateNewCommand gs ViewLocations
    else gs

print' :: GameState -> IO ()
print' = print

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'

  unless (input == ":quit") $
    print' (eval' input gs) >> loop' gs
