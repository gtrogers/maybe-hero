module MaybeHero.World
( World
, mkWorld
, moveRoom
, currentRoom
, lookupRoom
, lookupRoomStrict
, rooms
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified MaybeHero.Room as Room

type RoomName = String
data World = World (Map.Map RoomName Room.Room) RoomName

lookupRoomStrict :: String -> World -> Maybe Room.Room
lookupRoomStrict s (World roomMap _) = Map.lookup s roomMap

lookupRoom :: String -> World -> Room.Room
lookupRoom s world = Maybe.maybe message id (lookupRoomStrict s world)
  where message = (error $ "Room does not exist for name " ++ s)

moveRoom :: String -> World -> Maybe World
moveRoom direction world@(World roomMap _) = do
  newRoomName <- Map.lookup direction $ Room.roomOrientation $ currentRoom world
  return $ World roomMap newRoomName

currentRoom :: World -> Room.Room
currentRoom world@(World roomMap currentRoomName) = lookupRoom currentRoomName world

rooms :: World -> [Room.Room]
rooms (World roomMap _) = Map.elems roomMap

mkWorld :: (Map.Map RoomName Room.Room) -> RoomName -> World
mkWorld roomMap roomName = World roomMap roomName
