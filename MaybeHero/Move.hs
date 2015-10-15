module MaybeHero.Move (move) where

import qualified Data.Map as Map

import qualified MaybeHero.World as World
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Command as Command

move :: Command.Command

move [] oldWorld = World.updateLine oldWorld "I can't move without a direction"

move inputTokens oldWorld@(World.World room _) =
  case (Map.lookup (unwords inputTokens) (Room.roomOrientation room)) of
    Nothing -> World.updateLine oldWorld "You can't go that way..."
    Just room -> World.moveRoom oldWorld room
