module MaybeHero.Rooms
(lookupRoom
) where

import qualified Data.Map as Map
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Scenery as Scenery

roomMap :: [Room.Room] -> (Map.Map String Room.Room)
roomMap = Map.fromList . (map (\r -> (Room.roomName r, r)))

lookupRoom :: String -> [Room.Room] -> Room.Room
lookupRoom s rooms = case (Map.lookup s (roomMap rooms)) of
  (Just room) -> room
  _ -> error $ "Room does not exist for name " ++ s
