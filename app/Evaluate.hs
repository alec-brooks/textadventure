module Evaluate where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import State

getObject :: ObjectName -> GameState -> Maybe Object
getObject o gs = Map.lookup o $ objects l
  where
    l = locations gs Map.! currentLocation gs

getUseFn :: Maybe Object -> GameState -> GameState
getUseFn = Maybe.maybe (\g -> g {gameError = Just NoObject}) useFn

useItemObject :: ItemName -> ObjectName -> GameState -> GameState
useItemObject item object gs
  | Set.notMember item (inventory gs) = gs {gameError = Just NoItem}
  | Maybe.isNothing maybeObj = gs {gameError = Just NoObject}
  | otherwise = getUseFn maybeObj gs {command = Use item object, gameError = Nothing}
  where
    maybeObj = getObject object gs

eval:: Command -> GameState -> GameState
eval (GoTo newLocation) gs =
  if newLocation `elem` navigableLocations gs
    then
      gs
        { currentLocation = newLocation,
          command = GoTo newLocation,
          gameError = Nothing
        }
    else gs {gameError = Just UnableToGo}
eval (Interact object) gs =
  gs
    { command = Interact object,
      inventory = do
        let l = locations gs Map.! currentLocation gs
        let o = objects l Map.! object
        let i = item o
        case i of
          Nothing -> inventory gs
          Just item -> Set.insert item $ inventory gs,
      gameError = Nothing
    }
eval (Use item object) gs = useItemObject item object gs
eval cmd gs = gs {command = cmd, gameError = Nothing}

