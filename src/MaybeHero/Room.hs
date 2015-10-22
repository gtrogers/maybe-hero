module MaybeHero.Room
( Room
, describeRoom
, mkRoom
, roomOrientation
, showScenery
, showSceneryWithName
) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified MaybeHero.Scenery as Scenery

type RoomName = String
type Description = String
type Orientation = Map.Map String Room
data Room = Room RoomName Description Orientation [Scenery.Scenery]

describeRoom :: Room -> String
describeRoom (Room name description orientation _) =
  "[" ++ name ++ "]\n"
  ++ description
  ++ (showRoomTransitions orientation)

roomName :: Room -> String
roomName (Room name _ _ _) = name

roomScenery :: Room -> [Scenery.Scenery]
roomScenery (Room _ _ _ sceneryList) = sceneryList

roomOrientation :: Room -> Orientation
roomOrientation (Room _ _ orientation _) = orientation

mkRoom name description orientation sceneryList = Room name description orientation sceneryList

showRoomTransitions :: Orientation -> String
showRoomTransitions o = Map.foldWithKey addDesc "" o
  where addDesc k room str = str ++ "\n  " ++ k ++ "\t| " ++ (roomName room)

showScenery :: Room -> String
showScenery room =
  case (roomScenery room) of
    [] -> "There's nothing to see here"
    otherwise -> foldl (++) "" $ List.intersperse ", " $ map Scenery.sceneryName $ roomScenery room

showSceneryWithName :: Room -> String -> String
showSceneryWithName room name =
  case (roomScenery room) of
    [] -> "There's nothing to see here"
    otherwise -> Scenery.findDescription (roomScenery room) name

