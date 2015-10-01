import Data.Map as Map

import MaybeHero.Player
import MaybeHero.Room
import MaybeHero.Rooms
import MaybeHero.Move
import MaybeHero.Look
import MaybeHero.Help

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
      (x:xs) | x `elem` ["move","go","walk","run"] -> move xs oldPlayer
      (x:xs) | x == "help" -> help xs oldPlayer
      (x:xs) | x == "look" -> look xs oldPlayer
      _ -> updateLine oldPlayer $ "I don't know how to " ++ input

