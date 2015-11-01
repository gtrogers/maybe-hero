module MaybeHero.Rooms
( drawingRoom
, upstairsRoom
, dungeonRoom
, lookupRoom
) where

import qualified Data.Map as Map
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Scenery as Scenery

rooms = [drawingRoom, billiardsRoom, upstairsRoom, dungeonRoom]

roomMap = Map.fromList $ map (\r -> (Room.roomName r, r)) rooms

lookupRoom :: String -> Room.Room
lookupRoom s = case (Map.lookup s roomMap) of
  (Just room) -> room
  _ -> error $ "Room does not exist for name " ++ s

drawingRoom =
  Room.mkRoom
    "Drawing Room"
    "You see some faded drapes, an old sofa and a sleeping vicar."
    (Map.fromList [("north","The Dungeon"), ("up", "Upstairs"), ("east", "Billiards Room")])
    [Scenery.mkScenery
       "Faded Drapes"
       ["Drapes", "Curtains"]
       "The drapes are made of an old, faded velvet.",
     Scenery.mkScenery
       "Old Sofa"
       ["Sofa", "Couch"]
       "The sofa is green with white pinstripes, it sags decidedly to one side."]

billiardsRoom =
  Room.mkRoom
    "Billiards Room"
    "You see a sinister looking billiards table and a collection of empty brandy glasses."
    (Map.fromList [("west", "Drawing Room")])
    []

upstairsRoom =
  Room.mkRoom
    "Upstairs"
    "You are upstairs, it is dark and drafty here."
    (Map.fromList [("down", "Drawing Room")])
    []

dungeonRoom =
  Room.mkRoom
    "The Dungeon"
    "The dungeon is pleasantly central heated."
    (Map.fromList [("south", "Drawing Room")])
    []
