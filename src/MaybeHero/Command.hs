module MaybeHero.Command (
  Command
, look
, help
, move
) where

import qualified MaybeHero.World as World
import qualified MaybeHero.Room as Room
import qualified Data.Map as Map

type Command = [String] -> World.World -> World.World

look :: Command

look [] oldWorld@(World.World room _)
  = World.updateLine oldWorld $ Room.showScenery room
look wordList oldWorld@(World.World room _)
  = World.updateLine oldWorld $ Room.showSceneryWithName room (unwords wordList)

move :: Command

move [] oldWorld = World.updateLine oldWorld "I can't move without a direction"

move inputTokens oldWorld@(World.World room _) =
  case (Map.lookup (unwords inputTokens) (Room.roomOrientation room)) of
    Nothing -> World.updateLine oldWorld "You can't go that way..."
    Just room -> World.moveRoom oldWorld room

help :: Command

help inputTokens oldWorld =
  World.updateLine oldWorld $
    unwords $
      map (++ "\n")
        [" Move [direction] | Move in [direction]"
        ,"Look [something] | Take a closer look at [something]"]
