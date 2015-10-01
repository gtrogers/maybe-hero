import Data.Map as Map

import MaybeHero.Player
import MaybeHero.Room
import MaybeHero.Rooms

main = doGame player

doGame :: Player -> IO ()
doGame player = do
  putStrLn . (++ "\n") . nextLine $ player
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line player

gameLogic :: String -> Player -> Player
gameLogic direction oldPlayer@(Player (Room _ _ orientation) _) =
  case (Map.lookup direction orientation) of
      Nothing -> updateLine oldPlayer "You can't go that way..."
      Just room -> moveRoom oldPlayer room

player = Player drawingRoom (describeRoom drawingRoom)

