module Lib
(game) where

import Data.Map as Map

import qualified MaybeHero.World as World
import qualified MaybeHero.Input as Input
import qualified MaybeHero.Command as Command
import qualified MaybeHero.WorldLoader as Loader
import MaybeHero.Room
import MaybeHero.Rooms

world = World.mkWorld drawingRoom (describeRoom drawingRoom)
game = doGame world

doGame :: World.World -> IO ()
doGame world = do
  -- w <- Loader.parseWorldFromFile "world.yml"
  -- putStrLn $ describeRoom $ World.currentRoom w
  putStrLn . (++ "\n") . World.nextLine $ world
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line world

gameLogic :: String -> World.World -> World.World
gameLogic input oldWorld =
  case (Input.preProcess $ words input) of
      [] -> World.updateLine oldWorld "..."
      (x:xs) -> case (Input.wordToCommand x) of
                  (Just cmd) -> cmd xs oldWorld
                  Nothing    -> World.updateLine oldWorld $ "I don't know how to " ++ input
