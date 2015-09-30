main = doGame player

doGame :: Player -> IO ()
doGame player = do
  putStrLn . describeRoom . getCurrentRoom $ player 
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line player

gameLogic :: String -> Player -> Player
gameLogic "north" (Player (Room "drawing" _ _)) = Player dungeonRoom
gameLogic "south" (Player (Room "dungeon" _ _)) = Player drawingRoom
gameLogic _ oldPlayer = oldPlayer

data Player = Player Room | Ghost Room
  deriving Show
data Room = Room String String Orientation
data Orientation = Orientation (Maybe Room) (Maybe Room) (Maybe Room) (Maybe Room)
  deriving Show

instance Show Room where
  show (Room name _ _) = "Room " ++ name

northOnly r = Orientation (Just r) Nothing Nothing Nothing
southOnly r = Orientation Nothing Nothing (Just r) Nothing

describeRoom :: Room -> String
describeRoom (Room _ description orientation) = description ++ " " ++ show orientation

drawingRoom = Room "drawing" "Some more tea vicar?" (northOnly dungeonRoom) 
dungeonRoom = Room "dungeon" "Some more latex vicar?" (southOnly drawingRoom)

player = Player drawingRoom

getCurrentRoom :: Player -> Room
getCurrentRoom (Player room) = room
