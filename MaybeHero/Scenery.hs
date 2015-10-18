module MaybeHero.Scenery (
  Scenery
, mkScenery
, sceneryName
, sceneryDescription
, findDescription
) where

import qualified Data.Char as C

type SceneryName = String
type SceneryDescription = String
data Scenery = Scenery SceneryName SceneryDescription

mkScenery :: String -> String -> Scenery
mkScenery name description = Scenery name description

sceneryName :: Scenery -> String
sceneryName (Scenery name _) = name

sceneryDescription :: Scenery -> String
sceneryDescription (Scenery _ desc) = desc

findDescription :: [Scenery] -> SceneryName -> String
findDescription [] _ = "I can't see that here"
findDescription sceneryList name = 
  case (filter (\s -> (map C.toLower $ sceneryName s) == name) sceneryList) of 
    [] -> "I can't see that here"
    (x:xs) -> sceneryDescription x
