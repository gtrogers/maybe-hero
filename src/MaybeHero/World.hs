module MaybeHero.World
( World(..)
, mkWorld
, moveRoom
, currentRoom
, lookupRoom
) where

import qualified Data.Map as Map
import qualified MaybeHero.Room as Room

type RoomName = String
data World = World [Room.Room] RoomName

roomMap :: [Room.Room] -> (Map.Map String Room.Room)
roomMap = Map.fromList . (map (\r -> (Room.roomName r, r)))

lookupRoom :: String -> [Room.Room] -> Room.Room
lookupRoom s rooms = case (Map.lookup s (roomMap rooms)) of
  (Just room) -> room
  _ -> error $ "Room does not exist for name " ++ s

lookupRoomMaybe :: String -> [Room.Room] -> Maybe Room.Room
lookupRoomMaybe s = (Map.lookup s) . roomMap

moveRoom :: String -> World -> Maybe World
moveRoom direction (World rooms currentRoomName) = do
  currentRoom <- lookupRoomMaybe currentRoomName rooms
  newRoomName <- Map.lookup direction $ Room.roomOrientation currentRoom
  return $ World rooms newRoomName

currentRoom :: World -> Room.Room
currentRoom (World rooms currentRoomName) = lookupRoom currentRoomName rooms

mkWorld :: [Room.Room] -> RoomName -> World
mkWorld rooms roomName = World rooms roomName
