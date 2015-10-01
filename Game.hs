import Data.Map as Map

import MaybeHero.Player
import MaybeHero.Room
import MaybeHero.Rooms
import MaybeHero.Move

player = Player drawingRoom (describeRoom drawingRoom)
main = doGame player

doGame :: Player -> IO ()
doGame player = do
  putStrLn . (++ "\n") . nextLine $ player
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line player

gameLogic :: String -> Player -> Player
gameLogic input oldPlayer =
  case (words input) of
      [] -> updateLine oldPlayer "..."
      (x:xs) | x == "move" -> move xs oldPlayer
      _ -> updateLine oldPlayer $ "I don't know how to " ++ input

