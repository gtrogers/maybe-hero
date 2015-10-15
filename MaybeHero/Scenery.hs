module MaybeHero.Scenery (
  Scenery
, mkScenery
, sceneryName
) where

type SceneryName = String
type SceneryDescription = String
data Scenery = Scenery SceneryName SceneryDescription

mkScenery :: String -> Scenery
mkScenery name = Scenery name "A bit of scenery"

sceneryName :: Scenery -> String
sceneryName (Scenery name _) = name
