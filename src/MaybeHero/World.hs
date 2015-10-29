module MaybeHero.World
( World(..)
, mkWorld
, moveRoom
, currentRoom
, updateLine
, nextLine
) where

import qualified MaybeHero.Room as Room

data World = World Room.Room String

moveRoom :: World -> Room.Room -> World
moveRoom (World _ n) r = World r (Room.describeRoom r)

currentRoom :: World -> Room.Room
currentRoom (World room _) = room

nextLine :: World -> String
nextLine (World _ n) = n

mkWorld :: Room.Room -> String -> World
mkWorld room nextLine = World room nextLine

updateLine :: World -> String -> World
updateLine (World room _) n = World room n
