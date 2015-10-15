module MaybeHero.Rooms
( drawingRoom
, upstairsRoom
, dungeonRoom
) where

import qualified Data.Map as Map
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Scenery as Scenery

drawingRoom =
  Room.mkRoom
    "Drawing Room"
    "You see some faded drapes, an old sofa and a sleeping vicar."
    (Map.fromList [("north",dungeonRoom), ("up", upstairsRoom), ("east", billiardsRoom)])
    [Scenery.mkScenery "Faded Drapes",
     Scenery.mkScenery "Old Sofa"]

billiardsRoom =
  Room.mkRoom
    "Billiards Room"
    "You see a sinister looking billiards table and a collection of empty brandy glasses."
    (Map.fromList [("west", drawingRoom)])
    []

upstairsRoom =
  Room.mkRoom
    "Upstairs"
    "You are upstairs, it is dark and drafty here."
    (Map.fromList [("down",drawingRoom)])
    []

dungeonRoom =
  Room.mkRoom
    "The Dungeon"
    "The dungeon is pleasantly central heated."
    (Map.fromList [("south",drawingRoom)])
    []
