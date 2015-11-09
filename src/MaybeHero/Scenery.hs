module MaybeHero.Scenery (
  Scenery
, mkScenery
, sceneryName
, sceneryDescription
, findDescription
) where

import qualified Data.Char as C
import qualified MaybeHero.Utils as Utils
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type SceneryName = String
type SceneryDescription = String
type ScenerySynonyms = [String]
data Scenery = Scenery SceneryName SceneryDescription ScenerySynonyms

mkScenery :: SceneryName -> ScenerySynonyms -> SceneryDescription -> Scenery
mkScenery name syns description = Scenery name description syns

sceneryName :: Scenery -> String
sceneryName (Scenery name _ _) = name

sceneryDescription :: Scenery -> String
sceneryDescription (Scenery _ desc _) = desc

sceneryToSynTuple :: Scenery -> (Scenery, ScenerySynonyms)
sceneryToSynTuple scenery@(Scenery name _ syns) = (scenery, Utils.lowerCaseWords $ name:syns)

sceneryListToSynTuples :: [Scenery] -> [(Scenery, ScenerySynonyms)]
sceneryListToSynTuples = map sceneryToSynTuple

sceneryListToSceneryMap :: [Scenery] -> Map.Map String Scenery
sceneryListToSceneryMap = Utils.reverseAndExpandTuple . sceneryListToSynTuples

findScenery :: String -> [Scenery] -> Maybe Scenery
findScenery possibleSyn = Map.lookup possibleSyn . sceneryListToSceneryMap

findDescription :: String -> [Scenery] -> String
findDescription word = (Maybe.maybe "I can't see that here" sceneryDescription) . (findScenery word)
