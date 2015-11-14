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
import qualified Data.Maybe as Maybe
import qualified MaybeHero.Scenery as Scenery
import qualified MaybeHero.Inventory as I
import qualified MaybeHero.GameObject as GO
import qualified MaybeHero.Utils as Utils

type RoomName = String
type Direction = String
type Description = String
type Orientation = Map.Map Direction RoomName
data Room = Room { roomName :: RoomName
                  ,roomDescription :: Description
                  ,roomOrientation :: Orientation
                  ,roomScenery :: [Scenery.Scenery]
                  ,roomItems :: [I.MoveableItem]}

describeRoom :: Room -> String
describeRoom (Room name description orientation _ _) =
  "[" ++ name ++ "]\n"
  ++ description
  ++ (showRoomTransitions orientation)

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
    otherwise -> foldl (++) "" $ List.intersperse ", " $ itemNames
  where itemNames = map GO.objectName (roomScenery room) ++ map GO.objectName (roomItems room)

showSceneryWithName :: Room -> String -> String
showSceneryWithName room name =
  (Maybe.maybe "There's nothing to see here" id) $
   Utils.firstValue [GO.findDescription name (roomScenery room),
                     GO.findDescription name (roomItems room)]
