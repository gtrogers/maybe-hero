module MaybeHero.Rooms
( drawingRoom
, upstairsRoom
, dungeonRoom
) where

import Data.Map as Map

import MaybeHero.Room

drawingRoom = 
  Room
    "Drawing Room"
    "You see some faded drapes, an old sofa and a sleeping vicar."
    (Map.fromList [("north",dungeonRoom), ("up", upstairsRoom)])

upstairsRoom =
  Room
    "Upstairs"
    "You are upstairs, it is dark and drafty here."
    (Map.fromList [("down",drawingRoom)])

dungeonRoom =
  Room
    "The Dungeon"
    "The dungeon is pleasantly central heated."
    (Map.fromList [("south",drawingRoom)])

