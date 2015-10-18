import Data.Map as Map

import MaybeHero.World
import qualified MaybeHero.Input as Input
import MaybeHero.Room
import MaybeHero.Rooms
import MaybeHero.Move
import MaybeHero.Look
import MaybeHero.Help

world = World drawingRoom (describeRoom drawingRoom)
main = doGame world

doGame :: World -> IO ()
doGame world = do
  putStrLn . (++ "\n") . nextLine $ world
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line world

gameLogic :: String -> World -> World
gameLogic input oldWorld =
  case (Input.preProcess $ words input) of
      [] -> updateLine oldWorld "..."
      (x:xs) | x `elem` ["move","go","walk","run"] -> move xs oldWorld
      (x:xs) | x == "help" -> help xs oldWorld
      (x:xs) | x == "look" -> look xs oldWorld
      _ -> updateLine oldWorld $ "I don't know how to " ++ input

