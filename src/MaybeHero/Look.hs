module MaybeHero.Look (look) where

import qualified MaybeHero.Command as Command
import qualified MaybeHero.World as World
import qualified MaybeHero.Room as Room

look :: Command.Command

look [] oldWorld
  = World.updateLine oldWorld $ Room.showScenery room
  where room = World.currentRoom oldWorld
look wordList oldWorld
  = World.updateLine oldWorld $ Room.showSceneryWithName room (unwords wordList)
  where room = World.currentRoom oldWorld
