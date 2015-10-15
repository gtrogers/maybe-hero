module MaybeHero.Room
( Room
, describeRoom
, mkRoom
, roomOrientation
) where

import qualified Data.Map as Map

type RoomName = String
type Description = String
type Orientation = Map.Map String Room
data Room = Room RoomName Description Orientation


describeRoom :: Room -> String
describeRoom (Room name description orientation) =
  "[" ++ name ++ "]\n"
  ++ description
  ++ (showRoomTransitions orientation)

roomName :: Room -> String
roomName (Room name _ _) = name

roomOrientation :: Room -> Orientation
roomOrientation (Room _ _ orientation) = orientation

mkRoom = Room

showRoomTransitions :: Orientation -> String
showRoomTransitions o = Map.foldWithKey addDesc "" o
  where addDesc k room str = str ++ "\n  " ++ k ++ "\t| " ++ (roomName room)
