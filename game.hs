import Data.Map as Map

main = doGame player

doGame :: Player -> IO ()
doGame player = do
  putStrLn . getLastResult $ player
  putStrLn . describeRoom . getCurrentRoom $ player 
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line player

gameLogic :: String -> Player -> Player
gameLogic direction oldPlayer@(Player (Room _ _ orientation) _) =
  case (Map.lookup direction orientation) of
      Nothing -> updateResult oldPlayer "\nYou can't go that way..."
      Just room -> Player room ""

data Player = Player Room String
data Room = Room String String Orientation

type Orientation = Map.Map String Room

describeRoom :: Room -> String
describeRoom (Room name description orientation) = "[" ++ name ++ "]\n" ++ description ++ (roomTransitions orientation)

roomTransitions :: Orientation -> String
roomTransitions o = foldWithKey addDesc "" o
  where addDesc k (Room name _ _) str = str ++ "\n  " ++ k ++ " " ++ name

drawingRoom = Room "Drawing Room" "You see some faded drapes, an old sofa and a sleeping vicar." (Map.fromList [("north",dungeonRoom), ("up", upstairsRoom)])
upstairsRoom = Room "Upstairs" "You are upstairs, it is dark and drafty here." (Map.fromList [("down",drawingRoom)])
dungeonRoom = Room "The Dungeon" "The dungeon is pleasantly central heated." (Map.fromList [("south",drawingRoom)])

player = Player drawingRoom "Welcome to Maybe Hero!"

getCurrentRoom :: Player -> Room
getCurrentRoom (Player room _) = room
getLastResult :: Player -> String
getLastResult (Player _ lastActionResult) = lastActionResult
updateResult :: Player -> String -> Player
updateResult (Player room lastResult) s = Player room s

