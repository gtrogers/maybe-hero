module Lib
(game) where

import Data.Map as Map

import qualified MaybeHero.World as World
import qualified MaybeHero.Input as Input
import qualified MaybeHero.Command as Command
import MaybeHero.Room
import MaybeHero.Rooms

world = World.mkWorld drawingRoom (describeRoom drawingRoom)
game = doGame world

doGame :: World.World -> IO ()
doGame world = do
  putStrLn . (++ "\n") . World.nextLine $ world
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line world

gameLogic :: String -> World.World -> World.World
gameLogic input oldWorld =
  case (Input.preProcess $ words input) of
<<<<<<< HEAD
      [] -> updateLine oldWorld "..."
      (x:xs) -> case (Input.wordToCommand x) of
                  (Just cmd) -> cmd xs oldWorld
                  Nothing    -> updateLine oldWorld $ "I don't know how to " ++ input

=======
      [] -> World.updateLine oldWorld "..."
      (x:xs) | x `elem` ["move","go","walk","run"] -> move xs oldWorld
      (x:xs) | x == "help" -> help xs oldWorld
      (x:xs) | x == "look" -> look xs oldWorld
      _ -> World.updateLine oldWorld $ "I don't know how to " ++ input
>>>>>>> John | encapsulate World constructor
