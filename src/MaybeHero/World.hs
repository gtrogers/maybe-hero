module MaybeHero.World
( World
, mkWorld
, moveRoom
, currentRoom
, lookupRoom
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified MaybeHero.Room as Room

type RoomName = String
data World = World (Map.Map RoomName Room.Room) RoomName

lookupRoom :: String -> World -> Room.Room
lookupRoom s (World roomMap _) =
  Maybe.maybe (error $ "Room does not exist for name " ++ s) id (Map.lookup s roomMap)

moveRoom :: String -> World -> Maybe World
moveRoom direction world@(World roomMap _) = do
  newRoomName <- Map.lookup direction $ Room.roomOrientation $ currentRoom world
  return $ World roomMap newRoomName

currentRoom :: World -> Room.Room
currentRoom world@(World roomMap currentRoomName) = lookupRoom currentRoomName world

mkWorld :: (Map.Map RoomName Room.Room) -> RoomName -> World
mkWorld roomMap roomName = World roomMap roomName
