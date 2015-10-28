module MaybeHero.Input (
  preProcess
, wordToCommand
) where

import qualified Data.Map as Map
import qualified MaybeHero.Command as Command
import qualified MaybeHero.Utils as Utils

removeJunkWords :: [String] -> [String]
removeJunkWords words = filter (\w -> not $ w `elem` junkWords) words
  where junkWords = ["the", "a", "at"]


preProcess = Utils.lowerCaseWords . removeJunkWords


commandMap :: Map.Map String Command.Command
commandMap = Utils.reverseAndExpandTuple Command.getCommands

wordToCommand :: String -> Maybe Command.Command
wordToCommand word = Map.lookup word commandMap

