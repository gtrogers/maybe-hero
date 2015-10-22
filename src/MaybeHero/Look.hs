module MaybeHero.Look (look) where

import qualified MaybeHero.Command as Command
import qualified MaybeHero.World as World
import qualified MaybeHero.Room as Room

look :: Command.Command

look [] oldWorld@(World.World room _)
  = World.updateLine oldWorld $ Room.showScenery room
look wordList oldWorld@(World.World room _)
  = World.updateLine oldWorld $ Room.showSceneryWithName room (unwords wordList)
