module MaybeHero.World
( World(..)
, moveRoom
, updateLine
) where

import qualified MaybeHero.Room as Room

data World = World { room :: Room.Room, nextLine :: String }

moveRoom :: World -> Room.Room -> World
moveRoom (World _ n) r = World r (Room.describeRoom r)

updateLine :: World -> String -> World
updateLine (World room _) n = World room n
