module MaybeHero.Room
( Room
, describeRoom
, roomName
, mkRoom
, roomOrientation
, showScenery
, showSceneryWithName
) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified MaybeHero.Scenery as Scenery
import qualified MaybeHero.Inventory as I

type RoomName = String
type Direction = String
type Description = String
type Orientation = Map.Map Direction RoomName
data Room = Room RoomName Description Orientation [Scenery.Scenery] [I.MoveableItem]

describeRoom :: Room -> String
describeRoom (Room name description orientation _ _) =
  "[" ++ name ++ "]\n"
  ++ description
  ++ (showRoomTransitions orientation)

roomName :: Room -> String
roomName (Room name _ _ _ _) = name

roomScenery :: Room -> [Scenery.Scenery]
roomScenery (Room _ _ _ sceneryList _) = sceneryList

roomItems :: Room -> [I.MoveableItem]
roomItems (Room _ _ _ _ items) = items

roomOrientation :: Room -> Orientation
roomOrientation (Room _ _ orientation _ _) = orientation

mkRoom :: RoomName -> Description -> Orientation -> [Scenery.Scenery] -> [I.MoveableItem] ->  Room
mkRoom name description orientation sceneryList itemList =
  Room name description orientation sceneryList itemList

showRoomTransitions :: Orientation -> String
showRoomTransitions o = Map.foldWithKey addDesc "" o
  where addDesc k roomName str = str ++ "\n  " ++ k ++ "\t| " ++ roomName

showScenery :: Room -> String
showScenery room =
  case itemNames of
    [] -> "There's nothing to see here"
    otherwise -> foldl (++) "" $ List.intersperse ", " $ map Scenery.sceneryName $ roomScenery room
  where itemNames = map Scenery.sceneryName (roomScenery room) ++ map show (roomItems room)

showSceneryWithName :: Room -> String -> String
showSceneryWithName room name =
  case (roomScenery room) of
    [] -> "There's nothing to see here"
    otherwise -> Scenery.findDescription name (roomScenery room)
