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
              , (help, ["help"])
              , (inventory, ["inventory", "inv"])
              ]

look :: Command

look [] world = (world, output)
  where output = (Room.showScenery . World.currentRoom) world
look wordList world = (world, output)
  where output = Room.showSceneryWithName (World.currentRoom world) (unwords wordList)

move :: Command

move [] world = (world, "I can't move without a direction")
move inputTokens world =
  case (World.moveRoom (unwords inputTokens) world) of
    Nothing -> (world, "You can't go that way...")
    Just newWorld -> (newWorld, Room.describeRoom $ World.currentRoom newWorld)

help :: Command

help inputTokens world = (world, output)
  where output = unwords $ map (++ "\n")
                                    [" Move [direction] | Move in [direction]"
                                    ,"Look [something] | Take a closer look at [something]"]

inventory :: Command

inventory _ world = (world, World.showCurrentInventory world)
