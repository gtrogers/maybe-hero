module MaybeHero.World
( World(..)
, mkWorld
, moveRoom
, currentRoom
) where

import qualified MaybeHero.Room as Room
import qualified MaybeHero.Rooms as Rooms

type RoomName = String
data World = World RoomName

moveRoom :: World -> RoomName -> World
moveRoom world newRoomName = World newRoomName

currentRoom :: World -> Room.Room
currentRoom (World currentRoomName) = Rooms.lookupRoom currentRoomName

mkWorld :: RoomName -> World
mkWorld roomName = World roomName
