module MaybeHero.Input (
  preProcess
, wordToCommand
) where

import qualified Data.Char as C
import qualified MaybeHero.Command as Command
import qualified Data.Map as Map

removeJunkWords :: [String] -> [String]
removeJunkWords words = filter (\w -> not $ w `elem` junkWords) words
  where junkWords = ["the", "a", "at"]

toLower :: String -> String
toLower str = map C.toLower str

lowerCaseWords :: [String] -> [String]
lowerCaseWords wrds =  map toLower wrds

preProcess = lowerCaseWords.removeJunkWords

addSynonym :: Ord b => a -> Map.Map b a -> b -> Map.Map b a
addSynonym cmd m syn = Map.insert syn cmd m

addSynonyms :: Ord b => Map.Map b a -> (a, [b]) -> Map.Map b a
addSynonyms m (cmd, syns) = foldl (addSynonym cmd) m syns

mkCommandMap :: Ord b => [(a, [b])] -> Map.Map b a
mkCommandMap mappings = foldl addSynonyms Map.empty mappings

commandMap :: Map.Map String Command.Command
commandMap = mkCommandMap Command.getCommands

wordToCommand :: String -> Maybe Command.Command
wordToCommand word = Map.lookup word commandMap

