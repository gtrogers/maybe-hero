module Lib
(startGame) where

import Data.Map as Map

import qualified MaybeHero.World as World
import qualified MaybeHero.Input as Input
import qualified MaybeHero.Command as Command
import qualified MaybeHero.WorldLoader as Loader
import qualified MaybeHero.Room as Room
import qualified MaybeHero.Utils as Utils
import qualified Data.Maybe as Maybe
import qualified System.Environment as Env


startGame :: IO ()
startGame = do
  args <- Env.getArgs
  let fileName = Maybe.maybe (error "File argument must be supplied") id (Utils.headMaybe args)
  world <- Loader.parseWorldFromFile fileName
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
