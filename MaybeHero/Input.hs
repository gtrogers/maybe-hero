module MaybeHero.Input (
  removeJunkWords
) where

removeJunkWords :: [String] -> [String]
removeJunkWords words = filter (\w -> not $ w `elem` junkWords) words
  where junkWords = ["the", "a"]

