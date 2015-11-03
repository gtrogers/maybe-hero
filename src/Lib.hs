module Lib
(startGame) where

import Data.Map as Map

import qualified MaybeHero.World as World
import qualified MaybeHero.Input as Input
import qualified MaybeHero.Command as Command
import qualified MaybeHero.WorldLoader as Loader
import qualified MaybeHero.Room as Room


startGame :: IO ()
startGame = do
  world <- Loader.parseWorldFromFile "world.yml"
  let room = World.currentRoom world
  doGame (world, Room.describeRoom room)

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
