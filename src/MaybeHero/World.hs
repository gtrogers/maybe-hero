module MaybeHero.World
( World
, mkWorld
, moveRoom
, currentRoom
, lookupRoom
, lookupRoomStrict
, rooms
, roomDestinations
, showCurrentInventory
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Inventory as I
import qualified MaybeHero.GameObject as GO

type RoomName = String
type Inventory = [I.MoveableItem]
data World = World (Map.Map RoomName Room.Room) RoomName Inventory

mkWorld :: (Map.Map RoomName Room.Room) -> RoomName -> Inventory -> World
mkWorld roomMap roomName inventory = World roomMap roomName inventory

lookupRoomStrict :: String -> World -> Maybe Room.Room
lookupRoomStrict s (World roomMap _ _) = Map.lookup s roomMap

lookupRoom :: String -> World -> Room.Room
lookupRoom s world = Maybe.maybe message id (lookupRoomStrict s world)
  where message = (error $ "Room does not exist for name " ++ s)

moveRoom :: String -> World -> Maybe World
moveRoom direction world@(World roomMap _ inventory) = do
  newRoomName <- Map.lookup direction $ Room.roomOrientation $ currentRoom world
  return $ World roomMap newRoomName inventory

currentRoom :: World -> Room.Room
currentRoom world@(World roomMap currentRoomName _) = lookupRoom currentRoomName world

currentInventory :: World -> Inventory
currentInventory (World _ _ inventory) = inventory

showCurrentInventory :: World -> String
showCurrentInventory =  concat . List.intersperse ", " . map GO.objectName . currentInventory

rooms :: World -> [Room.Room]
rooms (World roomMap _ _) = Map.elems roomMap

roomDestinations :: World -> [String]
roomDestinations w = do
  room <- rooms w
  orientation <- Map.elems $ Room.roomOrientation room
  return orientation
