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
  }

data GameState = GameState
  { language :: Language,
    locations :: Map String Location
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
  loop' GameState {language = English, locations = locales}

read' :: IO String
read' =
  putStr "REPL> "
    >> hFlush stdout
    >> getLine

eval' :: String -> GameState -> String
eval' input gs = if input == "locations"
    then show (availableLocations $ locations gs)
    else input

print' :: String -> IO ()
print' = putStrLn

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'

  unless (input == ":quit") $
    print' (eval' input gs) >> loop' gs
