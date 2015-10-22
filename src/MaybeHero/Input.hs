module MaybeHero.Input (
  preProcess
) where

import qualified Data.Char as C

removeJunkWords :: [String] -> [String]
removeJunkWords words = filter (\w -> not $ w `elem` junkWords) words
  where junkWords = ["the", "a", "at"]

toLower :: String -> String
toLower str = map C.toLower str

lowerCaseWords :: [String] -> [String]
lowerCaseWords wrds =  map toLower wrds

preProcess = lowerCaseWords.removeJunkWords
