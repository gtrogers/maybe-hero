module MaybeHero.Command (
  Command
, look
, help
, move
, getCommands
) where

import qualified MaybeHero.World as World
import qualified MaybeHero.Room as Room
import qualified Data.Map as Map

type Command = [String] -> World.World -> (World.World, String)


getCommands :: [(Command, [String])]
getCommands = [ (look, ["look"])
              , (move, ["go", "move", "walk", "run"])
              , (help, ["help"])]

look :: Command

look [] world = (world, output)
  where output = (Room.showScenery . World.currentRoom) world
look wordList world = (world, output)
  where output = Room.showSceneryWithName (World.currentRoom world) (unwords wordList)

move :: Command

move [] world = (world, output)
  where output = "I can't move without a direction"

-- TODO some of the code below belongs in world
move inputTokens world@(World.World rooms roomName) =
  case (Map.lookup (unwords inputTokens) (Room.roomOrientation room)) of
    Nothing -> (world, unavailable)
    Just newRoomName -> (World.moveRoom world newRoomName, output)
                           where output = Room.describeRoom $ World.lookupRoom newRoomName rooms
  where room = World.currentRoom world
        unavailable = "You can't go that way..."

help :: Command

help inputTokens world = (world, output)
  where output = unwords $ map (++ "\n")
                                    [" Move [direction] | Move in [direction]"
                                    ,"Look [something] | Take a closer look at [something]"]
