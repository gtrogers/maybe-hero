module MaybeHero.Command (
  Command
, look
, help
, move
, getCommands
) where

import qualified MaybeHero.World as World
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Rooms as Rooms
import qualified Data.Map as Map

type Command = [String] -> World.World -> World.World


getCommands :: [(Command, [String])]
getCommands = [ (look, ["look"])
              , (move, ["go", "move", "walk", "run"])
              , (help, ["help"])]

look :: Command

look [] oldWorld@(World.World roomName _)
  = World.updateLine oldWorld $ Room.showScenery room
  where room = Rooms.lookupRoom roomName
look wordList oldWorld@(World.World roomName _)
  = World.updateLine oldWorld $ Room.showSceneryWithName room (unwords wordList)
  where room = Rooms.lookupRoom roomName

move :: Command

move [] oldWorld = World.updateLine oldWorld "I can't move without a direction"

move inputTokens oldWorld@(World.World roomName _) =
  case (Map.lookup (unwords inputTokens) (Room.roomOrientation room)) of
    Nothing -> World.updateLine oldWorld "You can't go that way..."
    Just newRoomName -> World.moveRoom oldWorld newRoomName
  where room = Rooms.lookupRoom roomName

help :: Command

help inputTokens oldWorld =
  World.updateLine oldWorld $
    unwords $
      map (++ "\n")
        [" Move [direction] | Move in [direction]"
        ,"Look [something] | Take a closer look at [something]"]
