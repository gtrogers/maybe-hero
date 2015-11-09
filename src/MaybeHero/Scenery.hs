module MaybeHero.Scenery (
  Scenery
, mkScenery
) where

import qualified MaybeHero.GameObject as GO

type SceneryName = String
type SceneryDescription = String
type ScenerySynonyms = [String]
data Scenery = Scenery SceneryName SceneryDescription ScenerySynonyms

mkScenery :: SceneryName -> ScenerySynonyms -> SceneryDescription -> Scenery
mkScenery name syns description = Scenery name description syns

instance GO.GameObject Scenery where
  objectName (Scenery name _ _) = name
  objectDescription (Scenery _ desc _) = desc
  objectSynonyms (Scenery _ _ syns) = syns
