module Lib 
(game) where

import Data.Map as Map

import MaybeHero.World
import qualified MaybeHero.Input as Input
import qualified MaybeHero.Command as Command
import MaybeHero.Room
import MaybeHero.Rooms

world = World drawingRoom (describeRoom drawingRoom)
game = doGame world

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
      (x:xs) -> case (Input.wordToCommand x) of
                  (Just cmd) -> cmd xs oldWorld
                  Nothing    -> updateLine oldWorld $ "I don't know how to " ++ input

