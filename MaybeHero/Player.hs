module MaybeHero.Player
( Player(..)
, moveRoom
, updateLine
) where

import MaybeHero.Room

data Player = Player { room :: Room, nextLine :: String }

moveRoom :: Player -> Room -> Player
moveRoom (Player _ n) r = Player r (describeRoom r)

updateLine :: Player -> String -> Player
updateLine (Player room _) n = Player room n

