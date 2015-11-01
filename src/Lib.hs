module Lib
(game) where

import Data.Map as Map

import qualified MaybeHero.World as World
import qualified MaybeHero.Rooms as Rooms
import qualified MaybeHero.Input as Input
import qualified MaybeHero.Command as Command
import qualified MaybeHero.WorldLoader as Loader
import MaybeHero.Room
import MaybeHero.Rooms

start = (World.mkWorld (roomName room), (describeRoom room))
  where room = Rooms.drawingRoom
game = doGame start

doGame :: (World.World, String) -> IO ()
doGame (world, output) = do
  putStrLn . (++ "\n") $ output
  line <- getLine
  case line of
    "exit" -> return ()
    _      -> doGame $ gameLogic line world

gameLogic :: String -> World.World -> (World.World, String)
gameLogic input world =
  case (Input.preProcess $ words input) of
      [] -> (world, "...")
      (x:xs) -> case (Input.wordToCommand x) of
                  (Just cmd) -> cmd xs world
                  Nothing    -> (world, "I don't know how to " ++ input)
