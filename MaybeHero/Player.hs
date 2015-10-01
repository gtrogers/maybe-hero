module MaybeHero.Player
( Player(..)
, moveRoom
, updateLine
) where

import MaybeHero.Room

data Player = Player { room :: Room, lastLine :: String }

moveRoom :: Player -> Room -> Player
moveRoom (Player _ lastLine) r = Player r lastLine

updateLine :: Player -> String -> Player
updateLine (Player room _) newLastLine = Player room newLastLine
