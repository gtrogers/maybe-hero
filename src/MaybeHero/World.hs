module MaybeHero.World
( World(..)
, mkWorld
, moveRoom
, currentRoom
) where

import qualified MaybeHero.Room as Room
import qualified MaybeHero.Rooms as Rooms

type RoomName = String
data World = World [Room.Room] RoomName

moveRoom :: World -> RoomName -> World
moveRoom (World rooms _) newRoomName = World rooms newRoomName

currentRoom :: World -> Room.Room
currentRoom (World rooms currentRoomName) = Rooms.lookupRoom currentRoomName rooms

mkWorld :: [Room.Room] -> RoomName -> World
mkWorld rooms roomName = World rooms roomName
