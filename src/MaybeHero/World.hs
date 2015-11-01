module MaybeHero.World
( World(..)
, mkWorld
, moveRoom
, currentRoom
, updateLine
, nextLine
) where

import qualified MaybeHero.Room as Room
import qualified MaybeHero.Rooms as Rooms

type RoomName = String
data World = World RoomName String

moveRoom :: World -> RoomName -> World
moveRoom world@(World _ n) newRoomName = World newRoomName (Room.describeRoom room)
 where room = Rooms.lookupRoom newRoomName

currentRoom :: World -> Room.Room
currentRoom (World currentRoomName _) = Rooms.lookupRoom currentRoomName

nextLine :: World -> String
nextLine (World _ n) = n

mkWorld :: RoomName -> String -> World
mkWorld roomName nextLine = World roomName nextLine

updateLine :: World -> String -> World
updateLine (World room _) n = World room n
